(ns hitch.graphs.immutable
  (:require [hitch.protocol :as proto]
            [clojure.set])
  #?(:clj
     (:import (clojure.lang ILookup)
              (java.util ArrayList)
              (java.io Writer))))

;; Tracing machinery
(def #?(:cljs    ^:dynamic ^boolean *trace*
        :default ^:dynamic *trace*)
  "Whether the ImmutableGraph will include a key ::trace in the command-result,
  which is a list of internal ops executed during the transaction. Default false."
  false)

(defonce ^:private op-history (volatile! []))

(defn- record! [op]
  (vswap! op-history conj op)
  nil)

(defn get-trace [] @op-history)

;; Utility functions

(defn subset? [superset subset]
  (reduce (fn [r x]
            (if (contains? superset x)
              r
              (reduced false)))
    true subset))

(defn- into! [xs! xs]
  (reduce conj! xs! xs))

;; Needed for transients: clj transients are callable but cljs are not.
;; Could use get, but less efficient
#?(:clj     (defn- lookup [^ILookup xs v] (.valAt xs v))
   :cljs    (defn- lookup [xs v] (-lookup xs v))
   :default (defn- lookup [xs v] (get xs v)))

(defn- update!
  ([xs! k f]
   (assoc! xs! k (f (lookup xs! k))))
  ([xs! k f x]
   (assoc! xs! k (f (lookup xs! k) x)))
  ([xs! k f x y]
   (assoc! xs! k (f (lookup xs! k) x y)))
  ([xs! k f x y z]
   (assoc! xs! k (f (lookup xs! k) x y z)))
  ([xs! k f x y z & args]
   (assoc! xs! k (apply f (lookup xs! k) x y z args))))

(defn- cempty? [xs]
  (zero? (count xs)))

(defn- not-cempty [xs]
  (if (cempty? xs)
    nil
    xs))

(defn- coerce-to-set [xs]
  (if (nil? xs)
    #{}
    (if (set? xs)
      xs
      (into #{} xs))))

;; UNKNOWN value sentinel

(defonce ^:private UNKNOWN
  (-> (reify
        #?@(:clj  [Object
                   (toString [_] "#<UNKNOWN>")]
            :cljs [IPrintWithWriter
                   (-pr-writer [_ writer opts]
                     (-write writer "#<UNKNOWN>"))]))
      #?(:clj (vary-meta assoc :type ::UNKNOWN))))

#?(:clj
   (defmethod print-method ::UNKNOWN [c, ^Writer w]
     (.write w "#<UNKNOWN>")))

(defn- unknown? [x] (identical? UNKNOWN x))

;; ILookup wrapper over graph state which provides selector values

(deftype GraphValues [selnodes]
  ILookup
  #?@(:clj  [(valAt [this sel]
               (.valAt this sel nil))
             (valAt [_ sel nf]
               (if-some [sn (.valAt ^ILookup selnodes sel)]
                 (let [v (.valAt ^ILookup sn :value)]
                   (if (unknown? v)
                     nf
                     v))
                 nf))]
      :cljs [(-lookup [this sel]
               (-lookup this sel nil))
             (-lookup [_ sel nf]
               (if-some [sn (-lookup selnodes sel)]
                 (let [v (-lookup sn :value)]
                   (if (unknown? v)
                     nf
                     v))
                 nf))]))

;; Internal ILookup wrapper combining dirty selnode state with previous state

(deftype MergedGraphValues [selnodes1 selnodes2]
  ILookup
  #?@(:clj  [(valAt [this sel]
               (.valAt this sel nil))
             (valAt [_ sel nf]
               (if-some [sn (.valAt ^ILookup selnodes1 sel)]
                 (let [v (.valAt ^ILookup sn :value)]
                   (if (unknown? v)
                     nf
                     v))
                 (if-some [sn (.valAt ^ILookup selnodes2 sel)]
                   (let [v (.valAt ^ILookup sn :value)]
                     (if (unknown? v)
                       nf
                       v))
                   nf)))]
      :cljs [(-lookup [this sel]
               (-lookup this sel nil))
             (-lookup [_ sel nf]
               (if-some [sn (-lookup selnodes1 sel)]
                 (let [v (-lookup sn :value)]
                   (if (unknown? v)
                     nf
                     v))
                 (if-some [sn (-lookup selnodes2 sel)]
                   (let [v (-lookup sn :value)]
                     (if (unknown? v)
                       nf
                       v))
                   nf)))]))

(deftype MergedLookup [m1 m2]
  ILookup
  #?@(:clj  [(valAt [this k]
               (.valAt this k nil))
             (valAt [_ k nf]
               (let [v (.valAt ^ILookup m1 k UNKNOWN)]
                 (if (unknown? v)
                   (let [v (.valAt ^ILookup m2 k UNKNOWN)]
                     (if (unknown? v)
                       nf
                       v))
                   v)))]
      :cljs [(-lookup [this k]
               (-lookup this k nil))
             (-lookup [_ k nf]
               (let [v (-lookup m1 k UNKNOWN)]
                 (if (unknown? v)
                   (let [v (-lookup m2 k UNKNOWN)]
                     (if (unknown? v)
                       nf
                       v))
                   v)))]))

;; SelectorNode

(defrecord ^:private SelectorNode
  [value int-children ext-children parents state])

(defn- new-SelectorNode []
  (assoc (->SelectorNode UNKNOWN #{} #{} #{} nil)
    :new? true
    :original-value UNKNOWN))

(defn- #?(:cljs    ^boolean no-children?
          :default no-children?)
  [sn]
  (and (cempty? (:int-children sn)) (cempty? (:ext-children sn))))

;; Internal reduction state manipulation functions

;;; Early Termination

(defn- throw-ex-info
  ([msg map]
   (throw (ex-info msg map)))
  ([msg map cause]
   (throw (ex-info msg map cause))))


;;; Effects

(defn- effect-queue []
  (volatile! (transient [])))

(defn- enq-effect! [effects effect]
  (when-not (nil? effect)
    (vswap! effects conj! effect)))

(defn- clear-effects! [effects]
  (let [es (persistent! @effects)]
    (vreset! effects (transient []))
    es))

(defn- comp-effects [effect-fns]
  (if (= (count effect-fns) 1)
    (first effect-fns)
    (fn [g] (run! (fn [f] (f g)) effect-fns))))

;;; Recalcs

(defn- recalc-queue []
  [(volatile! (transient []))                               ; pending recalcs
   (volatile! (transient #{}))                              ; dirty parents
   ])

(defn- enq-recalc! [[pending-recalc dirty-parents :as recalcs] parent]
  (vswap! pending-recalc conj! parent)
  (vswap! dirty-parents conj! parent)
  recalcs)

(defn- enq-recalc-children!
  [[pending-recalc dirty-parents :as recalcs] parent-sel child-sels]
  (vswap! pending-recalc into! child-sels)
  (vswap! dirty-parents conj! parent-sel)
  recalcs)

(defn- clear-recalcs! [[pending-recalc dirty-parents :as _recalcs]]
  (let [recalcs (into [] (distinct) (persistent! @pending-recalc))
        parents (persistent! @dirty-parents)]
    (vreset! pending-recalc (transient []))
    (vreset! dirty-parents (transient #{}))
    [recalcs parents]))

(defn- needs-recalc? [selnode old-selnode]
  (or
    ;; Brand new node
    #?(:default (:new? selnode) :cljs ^boolean (:new? selnode))
    ;; state change on non-vars
    (and (nil? (:machine selnode)) (not= (:state selnode) (:state old-selnode)))
    ;; went from no-children to have-children or vice-versa
    (not= (no-children? selnode) (no-children? old-selnode))))

(defn- recalc-sel-if-needed! [selnode sel old-selnode recalcs]
  ;; INVARIANT: All sel deps are known
  (if (needs-recalc? selnode old-selnode)
    (do
      (when *trace* (record! [:enq-recalc sel]))
      (enq-recalc! recalcs sel)
      (dissoc selnode :new?))
    (if #?(:default (:new? selnode)
           :cljs    ^boolean (:new? selnode))
      (dissoc selnode :new?)
      selnode)))


;;; Dependency (i.e. int-children) changes

(defn- ensure-add+del [add+del]
  (if (nil? add+del)
    [#{} #{}]
    add+del))

(defn- add+del->map [add+del]
  {:add (nth add+del 0)
   :del (nth add+del 1)})

(defn- apply-adds+dels [children adds+dels]
  ;; INVARIANT: Same item cannot be in both add and del
  (as-> (transient children) c
    (reduce disj! c (nth adds+dels 1))
    (reduce conj! c (nth adds+dels 0))
    (persistent! c)))

(defn- dep-changes->adds+dels [parents sel->add|rm]
  (let [^"[Ljava.util.ArrayList;" x
        (reduce-kv (fn [^"[Ljava.util.ArrayList;" r sel add?]
                     (if add?
                       (when-not (contains? parents sel)
                         (.add ^ArrayList (aget r 0) sel))
                       (when (contains? parents sel)
                         (.add ^ArrayList (aget r 1) sel)))
                     r)
          (doto (object-array 2)
            (aset 0 #?(:cljs (array-list) :clj (ArrayList.)))
            (aset 1 #?(:cljs (array-list) :clj (ArrayList.))))
          sel->add|rm)
        adds #?(:cljs (.toArray (aget x 0))
                :default (aget x 0))
        dels #?(:cljs (.toArray (aget x 1))
                :default (aget x 1))]
    [adds dels]))

(defn- get-adds [adds+dels]
  (nth adds+dels 0))

(letfn [(add-cmd [sel] [::proto/child-add sel])
        (del-cmd [sel] [::proto/child-del sel])]
  (defn- adds+dels->commands [adds+dels]
    (let [adds (nth adds+dels 0 #{})
          dels (nth adds+dels 1 #{})]
      (-> []
          (into (map del-cmd) dels)
          (into (map add-cmd) adds)))))

(defn- dep-changes []
  (volatile! (transient {})))

(defn- clear-deps! [deps]
  (let [r (persistent! @deps)]
    (vreset! deps (transient {}))
    r))

(defn- calculate-adds+dels [olds news]
  (when-not (= olds news)
    (let [^"[Ljava.lang.Object;" adds+dels
                    (reduce (fn [^"[Ljava.lang.Object;" a+d n]
                              (let [dels       (aget a+d 0)
                                    c-old-dels (count dels)
                                    dels'      (disj! dels n)]
                                (aset a+d 0 dels')
                                (when (= c-old-dels (count dels'))
                                  ;; only used in new
                                  (aset a+d 1 (conj! (aget a+d 1) n)))
                                a+d))
                      (doto (object-array 2)
                        (aset 0 (transient (if (set? olds)
                                             olds
                                             (into #{} olds))))
                        (aset 1 (transient []))) news)
          dels      (aget adds+dels 0)
          adds      (aget adds+dels 1)]
      (when-not (and (zero? (count adds)) (zero? (count dels)))
        [(persistent! adds) (persistent! dels)]))))

(letfn [(add-dep* [add+del add]
          (-> add+del ensure-add+del (update 0 conj add)))]
  (defn- add-dep [deps! parent-sel child-sel]
    (update! deps! parent-sel add-dep* child-sel)))

(letfn [(del-dep* [add+del del]
          (-> add+del ensure-add+del (update 1 conj del)))]
  (defn- del-dep [deps! parent-sel child-sel]
    (update! deps! parent-sel del-dep* child-sel)))

(defn- add-parent-deps [deps! child-sel adds+dels]
  (let [adds (nth adds+dels 0 #{})
        dels (nth adds+dels 1 #{})]
    (as-> deps! deps!
      (reduce #(del-dep %1 %2 child-sel) deps! dels)
      (reduce #(add-dep %1 %2 child-sel) deps! adds))))

(defn- enq-parent-deps! [volatile-deps child-sel parent-adds+dels]
  (vswap! volatile-deps add-parent-deps child-sel parent-adds+dels))


;;; Dirty node retrieval or creation

(defn- get-selnode [dirty-selnodes selnodes sel]
  (if-some [dn (lookup dirty-selnodes sel)]
    dn
    (when-some [n (selnodes sel)]
      (assoc n :original-value (:value n)))))

(defn- create-selnode! [sel effects]
  (when *trace* (record! [:create sel]))
  (let [sn (new-SelectorNode)]
    (cond->
      (cond
        (proto/var-selector? sel)
        (assoc sn :machine (proto/machine-selector sel))

        (proto/stateful-selector? sel)
        (let [{:keys [state effect]} (proto/create sel)]
          (when *trace* (when effect (record! [:enq-effect :create sel])))
          (enq-effect! effects effect)
          (when *trace* (record! [:state-oncreate sel state]))
          (assoc sn :state state))

        :else
        sn)

      ;; Machine's value is either ::machine-parents-known if no parents
      ;; or all parents are realized (not UNKNOWN).
      ;; This is a trick to ensure machines are not notified of
      ;; ::hp/parent-value-change unnecessarily.
      (proto/machine-selector? sel)
      (assoc :value ::machine-parents-known))))

(defn- get|create-selnode! [dirty-selnodes selnodes sel effects]
  (if-some [sn (get-selnode dirty-selnodes selnodes sel)]
    sn
    (create-selnode! sel effects)))

;;; Command application

(defn- fill-pending-commands [command-error cmds]
  (assoc command-error :pending-commands
                       (->> cmds
                            (drop-while #(not (identical? %
                                                (:bad-command command-error))))
                            next vec)))


(defn- apply-command* [sel acc cmd]
  (let [acc' (proto/command-step sel acc cmd)]
    (if (proto/command-error? acc')
      (reduced (assoc acc' :accumulator acc :bad-command cmd))
      acc')))

;; Very similar to h.g.graph-manager/apply-graph-node-commands
(defn- apply-commands* [selector state commands]
  (let [acc (reduce (fn [acc cmd] (apply-command* selector acc cmd))
              (proto/command-accumulator selector state)
              commands)]
    (if (proto/command-error? acc)
      (-> (fill-pending-commands acc commands)
          (assoc :selector selector :selector-state state))
      (let [state-effect-refresh (proto/command-result selector acc)]
        (if (proto/command-error? state-effect-refresh)
          (assoc state-effect-refresh
            :bad-command nil :pending-commands nil
            :accumulator acc)
          state-effect-refresh)))))

(defn- apply-commands [{old-state :state :as selnode} sel commands effects recalc]
  (let [r (apply-commands* sel old-state commands)]
    (if (proto/command-error? r)
      (let [err (assoc r :selector sel :selector-state old-state)]
        (throw-ex-info (if (nil? (:bad-command err))
                         "Selector command-result failed"
                         "Selector command")
          err))
      (let [{:keys [state effect recalc-child-selectors]} r]
        (when *trace*
          (when effect
            (record! [:enq-effect :command sel])))
        (enq-effect! effects effect)
        (when-not (empty? recalc-child-selectors)
          (when-not (proto/silent-selector? sel)
            (throw-ex-info "Selector is not a SilentSelector but returned recalc-child-selectors from a command-result"
              {:selector sel}))
          (when-not (subset? (:int-children selnode) recalc-child-selectors)
            (throw-ex-info "Selector attempted to recalculate a selector which is not its child."
              {:selector               sel
               :state                  (:state selnode)
               :children               (:int-children selnode)
               :bad-recalcs            (clojure.set/difference recalc-child-selectors (:int-children selnode))
               :commands               commands
               :recalc-child-selectors recalc-child-selectors})))
        (let [selnode' (-> (assoc selnode :state state)
                           ;; NOTE: Ensure a parent is recalced before children.
                           (recalc-sel-if-needed! sel selnode recalc))]
          (when *trace*
            (when-not (empty? recalc-child-selectors)
              (record! [:enc-recalcs :silent-selector-command sel recalc-child-selectors])))
          (enq-recalc-children! recalc sel recalc-child-selectors)
          selnode')))))

(defn- #?(:cljs    ^boolean var-belongs-to-machine?
          :default var-belongs-to-machine?)
  [ds selnodes machine-sel var-sel]
  (or
    (= machine-sel (:machine (lookup ds var-sel)))
    (= machine-sel (:machine (lookup selnodes var-sel)))
    (and
      (proto/var-selector? var-sel)
      (= machine-sel (proto/machine-selector var-sel)))))

(defn- merge-var-reset* [vr! ds selnodes machine-sel var-sel value]
  (if (var-belongs-to-machine? ds selnodes machine-sel var-sel)
    (assoc! vr! var-sel value)
    (throw-ex-info "Machines cannot var-reset non-vars or other machine's vars"
      {:machine       machine-sel
       :bad-var-reset [var-sel value]})))

(defn- merge-var-reset [old-vr ds selnodes machine-sel new-vr]
  (persistent!
    (reduce-kv (fn [vr! var-sel value]
                 (merge-var-reset* vr! ds selnodes machine-sel var-sel value))
      (transient (if (nil? old-vr) {} old-vr))
      new-vr)))

(defn- apply-machine-commands
  ([ds selnodes machine-sel commands effects recalcs deps]
   (apply-machine-commands ds selnodes
     (dissoc (get|create-selnode! ds selnodes machine-sel effects) :new?)
     machine-sel commands effects recalcs deps))
  ([ds selnodes sn machine-sel commands effects recalcs deps]
   (when *trace* (record! [:machine-commands commands]))
   (let [ro-graph  (->MergedGraphValues {machine-sel sn}
                     (->MergedLookup ds selnodes))
         r         (proto/apply-machine-commands machine-sel ro-graph sn commands)
         ;; NOTE: skipping :commands for now
         {:keys [state dep-change var-reset effect]} r
         sn'       (-> (assoc sn :state state)
                       ;; NOTE: keeping var-reset for the recalc phase of vars
                       ;; Var should look in here for its new value
                       (update :var-reset merge-var-reset ds selnodes
                         machine-sel var-reset))
         adds+dels (dep-changes->adds+dels (:parents sn') dep-change)
         sn'       (update sn' :parents apply-adds+dels adds+dels)]
     (when *trace*
       (let [[adds dels] adds+dels
             adds (into [] adds)
             dels (into [] dels)]
         (when-not (and (cempty? adds) (cempty? dels))
           (record! [:enq-parent-changes machine-sel {:adds adds :dels dels}]))))
     (enq-parent-deps! deps machine-sel adds+dels)
     (when *trace*
       (when (seq (keys var-reset))
         (record! [:enc-recalcs :var-reset machine-sel (keys var-reset)])))
     (enq-recalc-children! recalcs machine-sel (keys var-reset))

     (when *trace*
       (when effect
         (record! [:enq-effect :command machine-sel])))
     (enq-effect! effects effect)
     sn')))

(defn- apply-external-ops*-update-ext-children [sn sel update-ext-children]
  (when *trace* (record! [:update-ext-children sel update-ext-children]))
  (update sn :ext-children apply-adds+dels
    [(:add update-ext-children) (:del update-ext-children)]))

(defn- apply-external-ops*-apply-commands [sn sel commands effects recalcs]
  (when *trace* (record! [:commands sel commands]))
  (apply-commands sn sel commands effects recalcs))

(defn- apply-external-ops*
  [ds selnodes sel {:keys [::update-ext-children ::commands]} effects recalcs]
  (let [sn  (get|create-selnode! ds selnodes sel effects)
        sn' (cond-> sn
              (some? update-ext-children)
              (apply-external-ops*-update-ext-children sel update-ext-children))
        sn' (if (some? commands)
              ;; NOTE: Calls apply-commands which calls recalc-sel-if-needed
              ;; Need to delegate because parent should recalc before child,
              ;; but we only see state after StateEffect recalcs are added.
              (apply-external-ops*-apply-commands sn' sel commands effects recalcs)
              (recalc-sel-if-needed! sn' sel sn recalcs))]
    sn'))

(defn- apply-external-ops [dirty-selnodes selnodes effects recalcs deps sel->ops]
  ;; INVARIANT: sel->ops only updates external children or issues commands
  (persistent!
    (reduce-kv
      (fn [ds sel ops]
        (assoc! ds sel
          (if (proto/machine-selector? sel)
            ;; NOTE: Machines have no ext-deps, so ignore ::update-ext-children
            (apply-machine-commands ds selnodes sel (::commands ops) effects
              recalcs deps)
            (apply-external-ops* ds selnodes sel ops effects recalcs))))
      (transient dirty-selnodes)
      sel->ops)))

(defn- inform-selector [selnode sel adds+dels effects recalcs]
  (when-some [commands (not-cempty (adds+dels->commands adds+dels))]
    (when *trace* (record! [:inform-of-child-changes sel (add+del->map adds+dels)]))
    (apply-commands selnode sel commands effects recalcs)))

(defn- inform-machine-selector
  [ds selnodes selnode sel adds+dels effects recalcs deps]
  (when-some [commands (not-cempty (adds+dels->commands adds+dels))]
    (when *trace* (record! [:inform-of-child-changes sel (add+del->map adds+dels)]))
    (-> (apply-machine-commands ds selnodes selnode sel commands effects recalcs deps)
        (dissoc :new?))))

(defn- update-selnode-children [ds selnodes sel adds+dels effects recalcs deps]
  (when *trace* (record! [:update-int-children sel (add+del->map adds+dels)]))
  (let [sn  (get|create-selnode! ds selnodes sel effects)
        sn' (update sn :int-children apply-adds+dels adds+dels)
        sn' (cond
              ;; NOTE: inform*-selector calls apply*-commands which calls recalc-sel-if-needed
              ;; Need to delegate because parent should recalc before child,
              ;; but we only see state after StateEffect recalcs are added.
              (proto/machine-selector? sel)
              (inform-machine-selector ds selnodes sn' sel adds+dels effects recalcs deps)

              (proto/informed-selector? sel)
              (inform-selector sn' sel adds+dels effects recalcs)

              :else
              (recalc-sel-if-needed! sn' sel sn recalcs))
        sn' (cond-> sn'
              (and (:destroy? sn') (not (no-children? sn)))
              (dissoc :destroy?))]
    ;; Newly-added children need an opportunity to recalculate
    (when-not (or (unknown? (:value sn')) (proto/silent-selector? sel))
      ;; This is only reached if both
      ;; 1) a child recalced earlier (hence child sel guaranteed to be in
      ;;    dirty-selnodes)
      ;; 2) during the recalc it added current sel as a parent
      ;; When the child recalced and added us, it either resolved to a value
      ;; (thus needs no recalc), or it did not resolve and may need access to
      ;; our value.
      ;; This is basically a special case of "recalc my children after value
      ;; change" where some children were not known at the moment we recalced.
      (let [recalc-children (filterv #(unknown? (:value (lookup ds %)))
                              (get-adds adds+dels))]
        (when *trace*
          (when-not (cempty? recalc-children)
            (record! [:enq-recalcs :new-children sel recalc-children])))
        (enq-recalc-children! recalcs sel recalc-children)))
    (assoc! ds sel sn')))

(defn- update-selnodes-children [changes selnodes dirty-selnodes effects recalcs deps]
  (persistent!
    (reduce-kv
      (fn [ds sel adds+dels]
        (update-selnode-children ds selnodes sel adds+dels effects recalcs deps))
      (transient dirty-selnodes)
      changes)))

(defn- new-sel-value-var
  [varsel {old-value :value
           :keys     [int-children ext-children parents machine]} ds]
  (let [parents   (if (and (cempty? int-children) (cempty? ext-children))
                  #{}
                  (conj parents machine))
        new-value (-> (lookup ds machine) :var-reset (get varsel UNKNOWN))]
    (if-not (identical? UNKNOWN new-value)
      (proto/->SelectorValue new-value parents)
      (if-not (identical? UNKNOWN old-value)
        (proto/->SelectorValue old-value parents)
        (proto/->SelectorUnresolved parents)))))

(defn- #?(:cljs    ^boolean has-machines?
          :default has-machines?)
  [sels]
  (reduce
    (fn [_ s]
      (if (proto/machine-selector? s)
        (reduced true)
        false))
    false sels))

(defn- new-sel-value-selector [sel {:keys [state]} ds selnodes]
  (let [{:keys [parents] :as v}
        (proto/value sel (->MergedGraphValues ds selnodes) state)]
    (if (has-machines? parents)
      (throw-ex-info "Selectors cannot depend on machines"
        {:selector sel
         :parents  parents})
      v)))

(defn- new-sel-value [sel sn ds selnodes]
  (if (proto/var-selector? sel)
    (new-sel-value-var sel sn ds)
    (new-sel-value-selector sel sn ds selnodes)))

(defn- recalculate-node [sel ds selnodes effects recalcs deps]
  (when *trace* (record! [:recalc sel]))
  (let [sn               (get|create-selnode! ds selnodes sel effects)
        {:keys [parents] old-value :value} sn
        sv               (new-sel-value sel sn ds selnodes)
        new-parents      (coerce-to-set (:parents sv))
        unknown-value?   (proto/selector-unresolved? sv)
        new-value        (if unknown-value?
                           UNKNOWN
                           (:value sv))
        changed-value?   (and (not unknown-value?) (not= old-value new-value))
        recalc-children? (and changed-value? (not (proto/silent-selector? sel)))]
    (when recalc-children?
      ;; NOTE: Vars should not trigger recalcs on their own machines
      (let [dirty-children (disj (:int-children sn) (:machine sn))]
        (when *trace*
          (when-not (cempty? dirty-children)
            (record! [:enq-recalcs :value-change sel dirty-children])))
        (enq-recalc-children! recalcs sel dirty-children)))

    (when-some [parent-changes (calculate-adds+dels parents new-parents)]
      (when *trace*
        (record! [:enq-parent-changes sel {:adds (first parent-changes)
                                           :dels (second parent-changes)}]))
      (enq-parent-deps! deps sel parent-changes))
    (when *trace*
      (when changed-value?
        (record! [:value sel new-value])))
    (cond-> (-> sn
                (dissoc :new?)
                (assoc :parents new-parents))
      changed-value? (assoc :value new-value))))

(defn- #?(:cljs    ^boolean all-known?
          :default all-known?)
  [ds selnodes sels]
  (let [g (->MergedGraphValues ds selnodes)]
    (reduce (fn [_ sel]
              (if (unknown? (lookup g sel))
                (reduced false)
                true))
      true
      sels)))

(defn- add-machine-node-value [{:keys [parents] :as sn} ds selnodes]
  (if (all-known? ds selnodes parents)
    (assoc sn :value ::machine-parents-known)
    (assoc sn :value UNKNOWN)))

(defn- recalculate-machine-node [dirty-parents machine-sel ds selnodes effects recalcs deps]
  (when *trace* (record! [:recalc machine-sel]))
  (let [sn       (-> (get|create-selnode! ds selnodes machine-sel effects)
                     (dissoc :new?))
        commands (not-cempty
                   (into []
                     (keep #(when (contains? dirty-parents %)
                              [::proto/parent-value-change %]))
                     (:parents sn)))]
    (-> (if (some? commands)
          (apply-machine-commands ds selnodes sn machine-sel commands effects recalcs deps)
          sn)
        (add-machine-node-value ds selnodes))))

(defn- recalculate-nodes [dirty-parents recalc-sels dirty-selnodes selnodes effects recalcs deps]
  (persistent!
    (reduce (fn [ds sel]
              (assoc! ds sel
                (if (proto/machine-selector? sel)
                  (recalculate-machine-node dirty-parents sel ds selnodes effects recalcs deps)
                  (recalculate-node sel ds selnodes effects recalcs deps))))
      (transient dirty-selnodes)
      recalc-sels)))

(def ^:private filter-no-children
  (filter #(and (no-children? (val %)) (not (true? (:destroy? (val %)))))))

(def ^:private map-mark-destroy
  (map #(update % 1 assoc :destroy? true)))

(defn- mark-orphaned-nodes [dirty-selnodes selnodes]
  (cond-> (into {} (comp
                     filter-no-children
                     map-mark-destroy)
            dirty-selnodes)
    (some? selnodes) (into (comp
                             filter-no-children
                             (remove #(contains? dirty-selnodes (key %)))
                             (map #(assoc (val %) :original-value (:value (val %))))
                             map-mark-destroy)
                       selnodes)))

(defn- gc-sweep-node [sn sel deps]
  ;; INVARIANT: node has no children
  (let [{:keys [parents]} sn]
    (when *trace*
      (record! [:gc-sweep sel])
      (record! [:enq-parent-changes sel {:adds #{} :dels parents}]))
    (enq-parent-deps! deps sel [#{} parents])
    sn))

(defn- gc-sweep* [ds sel sn deps]
  (let [sn (gc-sweep-node sn sel deps)]
    (assoc! ds sel sn)))

(defn- gc-sweep [gc-marked dirty-selnodes deps]
  (persistent!
    (reduce-kv
      (fn [ds sel sn] (gc-sweep* ds sel sn deps))
      (transient dirty-selnodes)
      gc-marked)))

(defn- stabilize-nodes
  [dirty-selnodes selnodes effects recalcs deps full-gc? max-gc-cycles max-cycles]
  ;; all deps, then all recalcs
  ;; during deps, if informed, issue commands and update
  (if (zero? ^long max-cycles)
    (throw-ex-info "Could not stabilize graph: aborting transaction." {})
    (if-some [child-changes (not-cempty (clear-deps! deps))]
      (do
        (when *trace* (record! [:stabilize-children max-cycles]))
        (let [ds (update-selnodes-children child-changes selnodes dirty-selnodes
                   effects recalcs deps)]
          (recur ds selnodes effects recalcs deps full-gc? max-gc-cycles
            (dec ^long max-cycles))))
      (let [[need-recalc dirty-parents] (clear-recalcs! recalcs)]
        (if-not (cempty? need-recalc)
          (do
            (when *trace* (record! [:stabilize-values max-cycles]))
            (let [ds (recalculate-nodes dirty-parents need-recalc dirty-selnodes
                       selnodes effects recalcs deps)]
              (recur ds selnodes effects recalcs deps full-gc? max-gc-cycles
                (dec ^long max-cycles))))
          (if (pos? ^long max-gc-cycles)
            (do
              (when *trace* (record! [:gc-cycle max-gc-cycles (and full-gc? :full) max-cycles]))
              (let [gc-marked (mark-orphaned-nodes dirty-selnodes
                                (when (true? full-gc?) selnodes))]
                (if (cempty? gc-marked)
                  dirty-selnodes
                  (let [ds (gc-sweep gc-marked dirty-selnodes deps)]
                    (recur ds selnodes effects recalcs deps false
                      (dec ^long max-gc-cycles) max-cycles)))))
            dirty-selnodes))))))

(defn- destroy-orphaned-nodes
  [dirty-selnodes effects]
  (reduce-kv
    (fn [dn sel {:keys [destroy?] :as selnode}]
      (if (true? destroy?)
        (do
          (when *trace*
            (record! [:destroy sel]))
          (when (proto/stateful-selector? sel)
            (let [effect (proto/destroy sel (:state selnode))]
              (when *trace*
                (when effect
                  (record! [:enq-effect :destroy sel])))
              (enq-effect! effects effect)))
          (assoc dn sel nil))
        dn))
    dirty-selnodes
    dirty-selnodes))

(defn- update-ext-recalcs [ext-recalcs! ext-chs sel]
  (reduce (fn [m! ext-ch]
            (let [sels (if-some [sels (get m! ext-ch)]
                         sels
                         (transient []))]
              (assoc! m! ext-ch (conj! sels sel))))
    ext-recalcs! ext-chs))

(defn- ext-children-to-recalc
  [sel new-ext-children original-value value selnodes]
  (cond
    ;; No ext-children, no need to update
    (cempty? new-ext-children)
    []

    ;; value is different, definitely update all
    (not= value original-value)
    new-ext-children

    ;; value is the same; only recalc ext-children that are newly-added
    :else
    (persistent!
      (reduce disj!
        (transient new-ext-children)
        (:ext-children (selnodes sel))))))

(defn- merge-dirty-selnodes [dirty-selnodes selnodes]
  (let [ext-recalcs (volatile! (transient {}))
        val-changes (volatile! (transient {}))]
    [(->> dirty-selnodes
          (reduce-kv
            (fn [sns sel {:keys [value original-value] :as dirtynode}]
              (if (nil? dirtynode)
                (dissoc! sns sel)
                (assoc! sns sel
                  (let [unknown-value? (unknown? value)]
                    (when-not unknown-value?
                      (let [ext-chs (ext-children-to-recalc
                                      sel (:ext-children dirtynode)
                                      original-value value selnodes)]
                        (when-not (cempty? ext-chs)
                          (vswap! ext-recalcs update-ext-recalcs ext-chs sel)
                          (vswap! val-changes assoc! sel value))))
                    (cond-> (dissoc dirtynode :original-value :var-reset :destroy?)
                      ;; reuse old value when old and new are equal to preserve
                      ;; value identity as long as possible
                      (or unknown-value? (= value original-value))
                      (assoc :value original-value))))))
            (transient selnodes))
          persistent!)
     (persistent! @ext-recalcs)
     (persistent! @val-changes)]))

(defn- apply-ops [selnodes sel->ops gc-level effects]
  (let [pending-deps-changes (dep-changes)
        pending-recalcs      (recalc-queue)
        full-gc?             (= gc-level :full)
        max-gc-cycles        (cond
                               (and (number? gc-level) (not (neg? gc-level))) (long gc-level)
                               full-gc? 2147483647
                               :else 1)]
    (-> {}
        (apply-external-ops selnodes effects pending-recalcs pending-deps-changes sel->ops)
        (stabilize-nodes selnodes effects pending-recalcs pending-deps-changes full-gc? max-gc-cycles 1000)
        ;; INVARIANT: At this point, dep-changes and recalcs are empty
        (destroy-orphaned-nodes effects))))

(defn- im-child-add [acc [_ sel x]]
  (->> (update-in (-> acc :sel->cmd->arg (get sel {}))
         [::update-ext-children :add] (fnil conj #{}) x)
       (assoc-in acc [:sel->cmd->arg sel])))

(defn- im-child-del [acc [_ sel x]]
  (->> (update-in (-> acc :sel->cmd->arg (get sel {}))
         [::update-ext-children :del] (fnil conj #{}) x)
       (assoc-in acc [:sel->cmd->arg sel])))

(defn- im-child-adds-dels [acc [_ sel adds dels]]
  (as-> (:sel->cmd->arg acc) sca
    (reduce (fn [sca del-sel]
              (update-in sca [del-sel ::update-ext-children :del]
                (fnil conj #{}) sel))
      sca dels)
    (reduce (fn [sca add-sel]
              (update-in sca [add-sel ::update-ext-children :add]
                (fnil conj #{}) sel))
      sca adds)
    (assoc acc :sel->cmd->arg sca)))

(defn- im-child-unsub-all**
  [sel->cmd->arg sel {:keys [ext-children]} del-ext-children]
  (let [delable (into #{} (filter del-ext-children) ext-children)]
    (if (pos? (count delable))
      (update-in sel->cmd->arg [sel ::update-ext-children :del]
        (fnil into #{}) delable)
      sel->cmd->arg)))

(defn- im-child-unsub-all*
  [sel->cmd->arg selnodes del-ext-children]
  (reduce-kv
    (fn [sca sel node] (im-child-unsub-all** sca sel node del-ext-children))
    sel->cmd->arg selnodes))

(defn- im-child-unsub-all
  [{:keys [selnodes] :as acc} [_ del-ext-children]]
  (-> (update acc :sel->cmd->arg im-child-unsub-all* selnodes
        (into #{} del-ext-children))
      ;; gc more aggressively when mass-deleting
      (assoc :gc-level 20)))

(defn- im-command [acc [_ sel command]]
  (cond
    (not (vector? command))
    (proto/map->CommandError
      {:accumulator acc
       :error       "Malformed command; must be vector like `[:cmd-keyword & args]`"})

    (proto/machine-selector? sel)
    (proto/map->CommandError
      {:accumulator acc
       :error       "Machines cannot receive commands directly"})

    :else
    ;; Commands addressed to vars get re-addressed to machines
    (let [[sel' command']
          (if (proto/var-selector? sel)
            [(proto/machine-selector sel) [::proto/var-command sel command]]
            [sel command])]
      (->> (update (-> acc :sel->cmd->arg (get sel' {}))
             ::commands (fnil conj []) command')
           (assoc-in acc [:sel->cmd->arg sel'])))))

(defrecord ImmutableGraph [graph-id]
  proto/StatefulSelector
  (create [_] (proto/->StateEffect {} nil nil))
  (destroy [_ selnodes]
    ;; TODO: call destroy in topological order.
    ;; Probably means remove all ext-deps from stateful selectors then force gc
    ;; (once gc exists)
    (comp-effects
      (into []
        (keep (fn [[selector {:keys [state]}]]
                (when (proto/stateful-selector? selector)
                  (proto/destroy selector state))))
        selnodes)))
  proto/Selector
  (value [_ _ selnodes]
    (proto/->SelectorValue (->GraphValues selnodes) nil))

  proto/SilentSelector
  proto/InformedSelector
  proto/CommandableSelector
  (command-accumulator [_ selnodes]
    {:selnodes selnodes :sel->cmd->arg {} :gc-level 1})
  (command-step [_ acc [type :as command]]
    (case type
      ::do-gc (assoc acc :gc-level (second command))
      ::proto/child-add (im-child-add acc command)
      ::proto/child-del (im-child-del acc command)
      ::child-adds-dels (im-child-adds-dels acc command)
      ::children-del-all (im-child-unsub-all acc command)
      ::proto/command (im-command acc command)
      (proto/map->CommandError {:accumulator acc
                                :error       "Unrecognized command"})))
  (command-result [s acc]
    (when *trace*
      (vreset! op-history [[:ops (:sel->cmd->arg acc)]]))
    (let [{:keys [selnodes sel->cmd->arg gc-level]} acc
          sel->ops       sel->cmd->arg
          effects        (effect-queue)
          dirty-selnodes (apply-ops selnodes sel->ops gc-level effects)
          [new-selnodes recalcs val-changes] (merge-dirty-selnodes dirty-selnodes selnodes)]
      (cond->
        (proto/->StateEffect
          new-selnodes
          (when-some [effects (not-cempty (clear-effects! effects))]
            (comp-effects effects))
          (not-cempty (keys recalcs)))
        (pos? (count val-changes))
        (-> (assoc :observable-changed-selector-values val-changes)
            (assoc :selector-changes-by-ext-child
                   (persistent!
                     (reduce-kv (fn [r k v] (assoc! r k (persistent! v)))
                       (transient recalcs) recalcs))))))))
