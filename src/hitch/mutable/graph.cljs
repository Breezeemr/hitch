(ns hitch.mutable.graph
  (:require [hitch.oldprotocols :as oldproto]
            [goog.structs :refer [PriorityQueue]]
            [hitch.protocol :as proto]
            [hitch.mutable.node :as simple :refer [node NODE-NOT-RESOLVED-SENTINEL]]))

(def ^:dynamic *read-mode* false)
(def pending-actions (volatile! []))
(def scheduled-actions (volatile! false))

(defn schedule-gc [g]
  (when-some [timer (.peekKey (.-gc-list g))]
    (set! (.-cancel-gc g) (js/setTimeout (fn [] (.gc-pass g)) (- timer (.-current-time g))))))

(defn add-to-gc-list [g x]
  (set! (.-gc-list g) (.enqueue (.-gc-list g) (+ (.-gc-timer g) + (.-current-time g))  x))
  (when-not (.-cancel-gc g)
    (schedule-gc g)))

(defprotocol IBatching
  (-request-invalidations [graph invalidations])
  (peek-invalidations [graph])
  (take-invalidations! [graph]))

(defn get-external-dependents [graph selector]
  (when-let [n (get (.-nodemap graph) selector)]
    (.-external-dependencies n)
    )
  )
(defn get-or-create-node [graph data-selector]
  (if-let [n (get (.-nodemap graph) data-selector)]
    n
    (when (oldproto/eager-selector-resolve? graph)
      (oldproto/attempt-eager-selector-resolution! graph data-selector nil)
      (get (.-nodemap graph) data-selector))))



(defn get-temp-state [graph selector]
  (assert (satisfies? proto/CommandableSelector selector))
  (if-let [ts (get (.-tempstate graph) selector)]
    ts
    (let [node (get-or-create-node graph selector)
          ts   (atom (proto/command-accumulator selector (.-state node)))]
      (set! (.-tempstate graph) (assoc (.-tempstate graph) selector ts))
      ts)))

(declare schedule-actions)                                  ;invalidate-nodes normalize-tx!
(defn invalidate-external-items [graph ext-items]
  ;(prn "ext-items" ext-items)
  (run! (fn [changed-selector]
          (run! (fn [external-dep]
                  (oldproto/-change-notify external-dep))
            (get-external-dependents graph changed-selector)))
    ext-items))

(defn unused? [n]
  (and (nil? (not-empty (.-external-dependencies n))) (nil? (not-empty (.-subscribers n)))))

(declare -apply-selector-command)

(defn added-deps [graph child source filter]
  ;(prn "filtered-set-add" selector source filter)
  (run! (fn [parent]
          (when-not (filter parent)
            (let [parentnode  (get-or-create-node graph parent)
                  subscribers (.-subscribers parentnode)]
              (when-not (contains? subscribers child)
                ;(prn "add subscrption" child "to " parent)
                (set! (.-subscribers parentnode) (conj subscribers child))
                (when (satisfies? proto/InformedSelector parent)
                  (-apply-selector-command graph parent [:hitch.protocol/child-add child])))
              )))
    source))

(defn removed-deps [graph child source filter]
  ;(prn "filtered-set-add" selector source filter)
  (run! (fn [parent]
          (when-not (filter parent)
            (let [parentnode  (get-or-create-node graph parent)
                  subscribers (.-subscribers parentnode)]
              (when (contains? subscribers child)
                ;(prn "add subscrption" child "to " parent)
                (set! (.-subscribers parentnode) (disj subscribers child))
                (when (satisfies? proto/InformedSelector parent)
                  (-apply-selector-command graph parent [:hitch.protocol/child-del child])))
              (when (unused? parentnode)
                (add-to-gc-list graph parentnode)))))
    source))

(defn init-calc-node! [graph node]
  (let [selector (.-selector node)
        {new-value :value dependencies :parents :as vcontainer} (proto/value selector graph (.-state node))
        old-deps (.-refs node)]
    (set! (.-refs node) dependencies)
    ;(prn "dependencies " dependencies)
    ;inlineing invalidate produces error

    (cond
      (= (.-value node) new-value) nil
      (and (.-value node) (instance? proto/SelectorUnresolved vcontainer)) nil
      (not= (.-value node) new-value) (do
                                        ;(prn " value changed" vcontainer (type selector) (.-value node) new-value )
                                        (set! (.-value node) new-value)
                                        ;(run! #(-request-invalidations graph %) (.-subscribers node))
                                        ))
    (added-deps graph selector dependencies old-deps)
    (removed-deps graph selector old-deps dependencies)))
;;;; new api

(defn invalidate-selector-fn [graph]
  (fn [changed-items selector]
    (if-let [node (get (.-nodemap graph) selector)]
      (let [{new-value :value dependencies :parents :as vcontainer} (proto/value selector graph (.-state node))
            old-deps (.-refs node)]
        (set! (.-refs node) dependencies)
        ;(prn "vcontainer " vcontainer)
        ;inlineing invalidate produces error
        (added-deps graph selector dependencies old-deps)
        (removed-deps graph selector old-deps dependencies)
        (cond
          (= (.-value node) new-value) changed-items
          (and (.-value node) (instance? proto/SelectorUnresolved vcontainer)) changed-items
          (not= (.-value node) new-value) (do
                                            ;(prn " value changed" vcontainer(type selector) (.-value node) new-value )
                                            (set! (.-value node) new-value)

                                            ;:value-changed
                                            (conj! changed-items selector))))

      changed-items)))

(defn invalidate-level [graph selectors]
  (persistent! (reduce (invalidate-selector-fn graph) (transient #{}) selectors)))

(defn get-children-selectors [graph parents]
  (into #{} (mapcat (fn [parent]
                      (.-subscribers (get (.-nodemap graph) parent))))
    parents))

(defn invalidate-selectors [graph selectors]
  ;(prn "invalidate-nodes")
  (loop [changed-items (invalidate-level graph selectors) allchanged #{}]
    ;(prn "changed-items " changed-items)
    (if-let [children (not-empty (get-children-selectors graph changed-items))]
      (recur (invalidate-level graph children) (into allchanged changed-items))
      (into allchanged changed-items))))

(defn -apply-selector-command [graph selector effect]
  (let [state-atom (get-temp-state graph selector)]
    (swap! state-atom #(proto/command-step selector % effect))))

(defn finalize-commands [graph]
  (let [newstate-map (.-tempstate graph)]
    (set! (.-tempstate graph) {})
    (into [] (mapcat
               (fn [[selector v]]
                 ;(prn "selector v " selector v)
                 (if-let [node (get (.-nodemap graph) selector)]
                   (let [{new-state :state :as result} (proto/command-result selector @v)]
                     ;(prn  "new " new-state :recalc-child-selectors (:recalc-child-selectors result) )
                     (set! (.-state node) new-state)
                     (when-let [effect (:effect result)]
                       (schedule-actions graph)
                       (vswap! pending-actions conj effect))
                     (if (satisfies? proto/SilentSelector selector)
                       (conj (:recalc-child-selectors result) selector )
                       [selector]))
                   (prn "node not found"))))
      newstate-map)))

(defn -apply-selector-command-pairs [graph selector-command-pairs]
  (run! (fn [[selector command]]
          (-apply-selector-command graph selector command))
    selector-command-pairs))



(defn normalize-tx! [graph]
  (loop [changed-selectors #{}]
    ;(prn "invalidations " invalidations (.-tempstate graph))
    (if-some [new-invalids (not-empty (finalize-commands graph))]
      (recur (into changed-selectors (invalidate-selectors graph new-invalids)))
      (if-some [invalids (not-empty (take-invalidations! graph))]
        (recur (into changed-selectors (invalidate-selectors graph invalids)))
        (invalidate-external-items graph changed-selectors)))))



(defn process-actions [graph]
  (fn []
    (let [current-pending-actions @pending-actions
          simple-graph            (reify ILookup
                                    (-lookup [this k]
                                      (-lookup this k nil))
                                    (-lookup [this k not-found]
                                      (if-let [node (get (.-nodemap graph) k)]
                                        (.-value node)
                                        not-found)))]
      (vreset! scheduled-actions false)
      (vreset! pending-actions [])
      (run! (fn [scheduled-action]
              (scheduled-action graph (fn [selector-effect-pairs] (oldproto/apply-commands graph selector-effect-pairs))))
        current-pending-actions))))

(defn schedule-actions [graph]
  (when-not @scheduled-actions
    (vreset! scheduled-actions true)
    (goog.async.nextTick (process-actions graph)))
  )

(defn handle-resolution [node nf]
  (let [v (.-value node)]
    (if (identical? v NODE-NOT-RESOLVED-SENTINEL)
      nf
      v)))

;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable tempstate
                          ^:mutable gc-list ^:mutable gc-timer
                          ^:mutable cancel-gc ^:mutable current-time
                          ^:mutable internal-invalidated
                          ^:mutable external-invalidate!]
  Object
  (get-unresolved-selectors [_] (filter #(identical? (val (.-value %)) NODE-NOT-RESOLVED-SENTINEL) nodemap))
  (gc-pass [g]
    (let [pass-time (.getTime (js/Date.))]
      (set! current-time pass-time)
      (set! cancel-gc nil)
      (when
        (loop [selector-removed? false]
          (if (< (.peekKey gc-list) pass-time)
            (let [n (.remove gc-list)]
              (if (unused? n)
                (let [data-selector (.-selector n)]
                  (removed-deps g data-selector (.-refs n) #{})
                  (set! nodemap (dissoc nodemap data-selector))
                  (when (satisfies? proto/StatefulSelector data-selector)
                    (let [state (proto/destroy data-selector (.-state n))]
                      (when-let [effect (:effect state)]
                        (schedule-actions g)
                        (vswap! pending-actions conj effect))))
                  (recur true))
                (recur selector-removed?)))
            selector-removed?))
        (normalize-tx! g))))
  (gc-start [g gc-time]
    (set! gc-timer gc-time)
    (when-not cancel-gc
      (.gc-pass g)))
  (gc-pause [_]
    (when cancel-gc
      (js/clearTimeout cancel-gc))
    (set! gc-timer nil))
  ILookup
  (-lookup [o data-selector]
    (-lookup o data-selector nil))
  (-lookup [o data-selector not-found]
    (let [n (get nodemap data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
      (if (identical? n oldproto/NOT-IN-GRAPH-SENTINEL)
        not-found
        (let [val (.-value n)]
          (if (identical? val NODE-NOT-RESOLVED-SENTINEL)
            not-found
            val)))))
  oldproto/IEagerSelectorResolve
  (attempt-eager-selector-resolution! [graph data-selector nf]
    (let [n (get nodemap data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
      (if (identical? n oldproto/NOT-IN-GRAPH-SENTINEL)
        (let [new-node (if (satisfies? proto/StatefulSelector data-selector)
                         (let [state (proto/create data-selector)]
                           (when-let [effect (:effect state)]
                             (schedule-actions graph)
                             (vswap! pending-actions conj effect))
                           ;(run! #(-request-invalidations graph %) (:recalc-child-selectors state))
                           (node data-selector (:state state)))
                         (node data-selector nil))]

          (set! nodemap (assoc nodemap data-selector new-node))
          ;next two forms need to be resolved
          (init-calc-node! graph new-node)
          (handle-resolution new-node nf))
        (handle-resolution n nf))))
  oldproto/IDependencyGraph
  (apply-commands [graph selector-command-pairs]
    (-apply-selector-command-pairs graph selector-command-pairs)
    (set! (.-current-time graph) (.getTime (js/Date.)))
    (normalize-tx! graph))
  (update-parents [this child adds rms]
    ;(prn "update-parents " child adds rms )
    (set! (.-current-time graph) (.getTime (js/Date.)))
    (normalize-tx! this)
    (doseq [add adds
            :let [n (get nodemap add)]
            :when n]
      (set! (.-external-dependencies n) ((fnil conj #{}) (.-external-dependencies n) child)))
    (doseq [rm rms
            :let [n (get nodemap rm)]
            :when n]
      (let [new-external-deps (not-empty (disj (.-external-dependencies n) child))]
        (when (and (nil? new-external-deps) (nil? (not-empty (.-subscribers n))))
          (add-to-gc-list this n))
        (set! (.-external-dependencies n) new-external-deps))))
  IBatching
  (-request-invalidations [graph invalidaiton]
    (if internal-invalidated
      (set! internal-invalidated (conj! internal-invalidated invalidaiton))
      (set! internal-invalidated (transient [invalidaiton]))))
  (peek-invalidations [graph]
    internal-invalidated)
  (take-invalidations! [graph]
    (when-let [ret internal-invalidated]
      (set! internal-invalidated nil)
      (persistent! ret))))

(defn graph []
  (DependencyGraph. {} {} (PriorityQueue.) nil nil nil identity))

(defn get-node-map [graph]
  (.-nodemap graph))


