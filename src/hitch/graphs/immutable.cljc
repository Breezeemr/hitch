(ns hitch.graphs.immutable
  (:require [hitch.protocol :as proto])
  #?(:clj
     (:import (clojure.lang ILookup))))

;; Tracing machinery
(def #?(:default ^:dynamic *trace*
        :cljs    ^:dynamic ^boolean *trace*)
  "Whether the ImmutableGraph will include a key ::trace in the command-result,
  which is a list of internal ops executed during the transaction. Default false."
  true)


(def ^:dynamic *op-history* (volatile! []))

(defn- record! [sel op]
  (when *trace*
    (vswap! *op-history* conj [sel op]))
  nil)

;; Utility functions

(defn subset? [superset subset]
  (reduce (fn [r x]
            (if (contains? superset x)
              r
              (reduced false)))
    true subset))

(defn- into! [xs! xs]
  (reduce conj! xs! xs))

(defn- update!
  ([xs! k f]
   (assoc! xs! k (f (xs! k))))
  ([xs! k f x]
   (assoc! xs! k (f (xs! k) x)))
  ([xs! k f x y]
   (assoc! xs! k (f (xs! k) x y)))
  ([xs! k f x y z]
   (assoc! xs! k (f (xs! k) x y z)))
  ([xs! k f x y z & args]
   (assoc! xs! k (apply f (xs! k) x y z args))))

(defn- cempty? [xs]
  (zero? (count xs)))

(defn- coerce-to-set [xs]
  (if (nil? xs)
    #{}
    (if (set? xs)
      xs
      (into #{} xs))))

;; UNKNOWN value sentinel

(defonce ^:private UNKNOWN #?(:clj  (Object.)
                              :cljs #js{}))

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

;; SelectorNode

(defrecord ^:private SelectorNode
  [value int-children ext-children parents state])

(defn- new-SelectorNode []
  (assoc (->SelectorNode UNKNOWN #{} #{} #{} nil)
    :new? true
    :original-value UNKNOWN))

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
  (fn [g] (run! (fn [f] (f g)) effect-fns)))

;;; Recalcs

(defn- recalc-queue []
  (volatile! (transient [])))

(defn- enq-recalc! [recalcs sel]
  (record! sel ::enq-recalc)
  (vswap! recalcs conj! sel))

(defn- enq-recalcs! [recalcs sels]
  (record! nil [::enq-recalcs sels])
  (vswap! recalcs into! sels))

(defn- clear-recalcs! [recalcs]
  (let [sels (into [] (distinct) (persistent! @recalcs))]
    (vreset! recalcs (transient []))
    (record! nil [::clear-recalcs sels])
    sels))

(defn- needs-recalc? [sel selnode old-state]
  (and
    (or #?(:default (:new? selnode)
           :cljs    ^boolean (:new? selnode))
      (not= old-state (:state selnode)))
    (or (not (cempty? (:ext-children selnode)))
      (and (not (cempty? (:int-children selnode)))
        (not (satisfies? proto/SilentSelector sel))))))

(defn- recalc-sel-if-needed! [selnode sel old-state recalcs]
  ;; INVARIANT: All sel deps are known
  (record! sel [::recalc-sel? selnode sel old-state])
  (if (needs-recalc? sel selnode old-state)
    (do
      (enq-recalc! recalcs sel)
      (assoc selnode :new? false))
    (if #?(:default (:new? selnode)
           :cljs    ^boolean (:new? selnode))
      (assoc selnode :new? false)
      selnode)))


;;; Dependency (i.e. int-children) changes

(defn- ensure-add+del [add+del]
  (if (nil? add+del)
    [#{} #{}]
    add+del))

(defn- apply-adds+dels [children adds+dels]
  ;; INVARIANT: Same item cannot be in both add and del
  (as-> (transient children) c
    (reduce disj! c (nth adds+dels 1))
    (reduce conj! c (nth adds+dels 0))
    (persistent! c)))

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

;; TODO: likely also a utility function
(defn- calculate-adds+dels [olds news]
  (when-not (= olds news)
    (let [adds+dels (reduce (fn [a+d n]
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
  (if-some [dn (dirty-selnodes sel)]
    dn
    (when-some [n (selnodes sel)]
      (assoc n :original-value (:value n)))))

(defn- create-selnode! [sel effects]
  (record! sel [::create])
  (let [sn (new-SelectorNode)]
    (if (satisfies? proto/StatefulSelector sel)
      (let [{:keys [state effect]} (proto/create sel)]
        (enq-effect! effects effect)
        (assoc sn :state state))
      sn)))

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

;; TODO: Candidate for a public utility function?
;; Very similar to h.g.graph-manager/apply-graph-node-commands
(defn- apply-commands* [selector state commands]
  (let [acc (reduce (fn [acc cmd] (apply-command* selector acc cmd))
              (proto/command-accumulator commands state)
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

(defn- apply-commands [{:keys [state] :as selnode} sel commands effects recalc]
  (let [r (apply-commands* sel state commands)]
    (if (proto/command-error? r)
      (let [err (assoc sel :selector sel :selector-state state)]
        (throw-ex-info (if (nil? (:bad-command err))
                         "Selector command-result failed"
                         "Selector command")
          err))
      (let [{:keys [state effect recalc-child-selectors]} r]
        (enq-effect! effects effect)
        (when-not (empty? recalc-child-selectors)
          (when-not (satisfies? proto/SilentSelector sel)
            (throw-ex-info "Selector is not a SilentSelector but returned recalc-child-selectors from a command-result"
              {:selector sel}))
          (when-not (subset? (:int-children sel) recalc-child-selectors)
            (throw-ex-info "Selector attempted to recalculate a selector which is not its child."
              {:selector               sel
               :state                  (:state selnode)
               :children               (:int-children selnode)
               :commands               commands
               :recalc-child-selectors recalc-child-selectors}))
          (enq-recalcs! recalc recalc-child-selectors))
        (assoc selnode :state state)))))

(defn- apply-external-ops [dirty-selnodes selnodes effects recalcs sel->ops]
  ;; INVARIANT: sel->ops only updates external children or issues commands
  (persistent!
    (reduce-kv
      (fn [ds sel {:keys [::update-ext-children ::commands]}]
        (let [sn  (get|create-selnode! ds selnodes sel effects)
              sn' (cond-> sn
                    (some? update-ext-children)
                    (update :ext-children apply-adds+dels
                      [(:add update-ext-children) (:del update-ext-children)])

                    (some? commands)
                    (apply-commands sel commands effects recalcs))]
          (assoc! ds sel (recalc-sel-if-needed! sn' sel (:state sn) recalcs))))
      (transient dirty-selnodes)
      sel->ops)))

(defn- inform-selector [selnode sel adds+dels effects recalcs]
  (when-some [commands (not-empty (adds+dels->commands adds+dels))]
    (apply-commands selnode sel commands effects recalcs)))

(defn- update-selnodes-children [changes selnodes dirty-selnodes effects recalcs]
  (persistent!
    (reduce-kv
      (fn [ds sel adds+dels]
        (let [sn  (get|create-selnode! ds selnodes sel effects)
              sn' (cond-> (update sn :int-children apply-adds+dels adds+dels)
                    (proto/informed-selector? sel)
                    (inform-selector sel adds+dels effects recalcs))]
          (assoc! ds sel (recalc-sel-if-needed! sn' sel (:state sn) recalcs))))
      (transient dirty-selnodes)
      changes)))

(defn- recalculate-node [sel dirty-selnodes selnodes effects recalcs deps]
  (let [sn               (get|create-selnode! dirty-selnodes selnodes sel effects)
        sn               (dissoc sn :new?)                  ;We recalc immediately, so does not matter if new
        {:keys [state parents] old-value :value} sn
        sv               (proto/value sel (->MergedGraphValues dirty-selnodes selnodes) state)
        new-parents      (coerce-to-set (:parents sv))
        unknown-value?   (proto/selector-unresolved? sv)
        new-value        (if unknown-value?
                           UNKNOWN
                           (:value sv))
        changed-value?   (and (not unknown-value?) (not= old-value new-value))
        recalc-children? (and changed-value? (not (proto/silent-selector? sel)))]
    (when recalc-children?
      (enq-recalcs! recalcs (:int-children sn)))
    (when-some [parent-changes (calculate-adds+dels parents new-parents)]
      (enq-parent-deps! deps sel parent-changes))
    (cond-> (assoc sn :parents new-parents)
      changed-value? (assoc :value new-value))))

(defn- recalculate-nodes [recalc-sels dirty-selnodes selnodes effects recalcs deps]
  (persistent!
    (reduce (fn [ds sel]
              (assoc! ds sel (recalculate-node sel ds selnodes effects recalcs deps)))
      (transient dirty-selnodes)
      recalc-sels)))

(defn- stabilize-nodes [dirty-selnodes selnodes effects recalcs deps max-cycles]
  ;; all deps, then all recalcs
  ;; during deps, if informed, issue commands and update
  (if (zero? max-cycles)
    (throw-ex-info "Could not stabilize graph: aborting transaction." {})
    (if-some [child-changes (not-empty (clear-deps! deps))]
      (let [ds (update-selnodes-children child-changes selnodes dirty-selnodes effects recalcs)]
        (recur ds selnodes effects recalcs deps (dec max-cycles)))
      (if-some [need-recalc (not-empty (clear-recalcs! recalcs))]
        (let [ds (recalculate-nodes need-recalc dirty-selnodes selnodes effects recalcs deps)]
          (recur ds selnodes effects recalcs deps (dec max-cycles)))
        dirty-selnodes))))

(defn- destroy-orphaned-nodes
  [dirty-selnodes effects]
  (reduce-kv
    (fn [dn sel {:keys [int-children ext-children state] :as selnode}]
      (if (and (zero? (count int-children)) (zero? (count ext-children)))
        (do
          (when (satisfies? proto/StatefulSelector sel)
            (enq-effect! effects (proto/destroy sel state)))
          (assoc dn sel nil))
        dn))
    dirty-selnodes
    dirty-selnodes))

(defn- merge-dirty-selnodes [dirty-selnodes selnodes]
  (let [ext-recalcs (volatile! (transient #{}))]
    [(->> dirty-selnodes
          (reduce-kv
            (fn [sns sel {:keys [value original-value] :as dirtynode}]
              (if (nil? sel)
                (dissoc! sns sel)
                (assoc! sns sel
                  (if (= original-value value)
                    (-> (dissoc dirtynode :original-value)
                        (assoc :value original-value))
                    (do
                      (when-not (unknown? value)
                        (vswap! ext-recalcs into! (:ext-children dirtynode)))
                      (dissoc dirtynode :original-value))))))
            (transient selnodes))
          persistent!)
     (persistent! @ext-recalcs)]))

(defn- apply-ops [selnodes sel->ops effects]
  (let [pending-deps-changes (dep-changes)
        pending-recalcs      (recalc-queue)]
    (-> {}
        (apply-external-ops selnodes effects pending-recalcs sel->ops)
        (stabilize-nodes selnodes effects pending-recalcs pending-deps-changes 1000)
        ;; INVARIANT: At this point, dep-changes and recalcs are empty
        (destroy-orphaned-nodes effects))))

(defn- prepare-ops [selnodes sel->cmd->arg]
  (reduce-kv
    (fn [sel->ops sel cmd->arg]
      (let [selnode (get selnodes sel)]
        (if (and (nil? selnode)
              (empty? (-> cmd->arg ::update-ext-children :add)))
          ;; Degenerate case: if we never add an external child to a node that
          ;; does not exist, no need to produce any ops for the node.
          (dissoc sel->ops sel)
          sel->ops)))
    sel->cmd->arg
    sel->cmd->arg))

(defrecord ImmutableGraph [graph-id]
  proto/StatefulSelector
  (create [s] (proto/->State {}))
  (destroy [s state]
    ; TODO: tx to destroy all destroyable selectors
    nil
    )
  proto/Selector
  (value [s _ selnodes]
    (proto/->SelectorValue (->GraphValues selnodes) nil))

  proto/SilentSelector
  proto/InformedSelector
  proto/CommandableSelector
  (command-accumulator [s selnodes]
    {:selnodes selnodes :sel->cmd->arg {}})
  (command-step [s acc [type sel x :as command]]
    ;; TODO: Validate commands
    (let [seas (-> acc :sel->cmd->arg (get sel {}))]
      (case type
        ::proto/child-add
        (->> (update-in seas [::update-ext-children :add] (fnil conj #{}) x)
             (assoc-in acc [:sel->cmd->arg sel]))

        ::proto/child-del
        (->> (update-in seas [::update-ext-children :del] (fnil conj #{}) x)
             (assoc-in acc [:sel->cmd->arg sel]))

        ::child-adds-dels
        (->> (-> seas
                 (update-in [::update-ext-children :del] (fnil into #{})
                   (nth command 3))
                 (update-in [::update-ext-children :add] (fnil into #{}) x))
             (assoc-in acc [:sel->cmd->arg sel]))

        ::proto/command
        (do (assert (vector? x) "Commands must be vectors like `[:command-keyword & args]`.")
            (->> (update seas ::commands (fnil conj []) x)
                 (assoc-in acc [:sel->cmd->arg sel])))

        (proto/map->CommandError {:accumulator acc
                                  :error       "Unrecognized command"}))))
  (command-result [s acc]
    (let [{:keys [selnodes sel->cmd->arg]} acc
          sel->ops       (prepare-ops selnodes sel->cmd->arg)
          effects        (effect-queue)
          dirty-selnodes (apply-ops selnodes sel->ops effects)
          [new-selnodes recalcs] (merge-dirty-selnodes dirty-selnodes selnodes)]
      (proto/->StateEffectRefresh
        new-selnodes
        (when-some [effects (not-empty (clear-effects! effects))]
          (comp-effects effects))
        (not-empty recalcs)))))
