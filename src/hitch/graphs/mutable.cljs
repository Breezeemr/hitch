(ns hitch.graphs.mutable
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.nodes.simple :refer [node]]))

(declare   schedule-actions)                                  ;invalidate-nodes normalize-tx!
(defn invalidate-external-items [graph ext-items]
  (run! (fn [changed-selector]
          (run! (fn [external-dep]
                  (oldproto/-change-notify external-dep graph changed-selector))
                (oldproto/-get-external-dependents (get graph changed-selector))))
        ext-items))


(defn filtered-set-add [target selector source filter ]
  ;(prn "filtered-set-add" selector source filter)
  (conj! target [selector (eduction (remove filter) source)]))

(declare -apply-selector-effect)
(defn update-dependencies! [graph newdeps retiredeps]
  ;(prn "newdeps retiredeps" newdeps retiredeps)
  (run!
    (fn [[child parents]]
      (run! (fn [parent]
              (let [parentnode (oldproto/get-or-create-node graph parent)
                    subscribers (.-subscribers parentnode)]
                (when-not (contains? subscribers child)
                  ;(prn "add subscrption" child "to " parent)
                  (set! (.-subscribers parentnode) (conj subscribers child)))
                (when (satisfies? oldproto/InformedSelector parent)
                  (-apply-selector-effect graph parent [:add-dep child]))
                ))
            parents))
    newdeps)
  (run!
    (fn [[child parents]]
      (run! (fn [parent]
              (let [parentnode (oldproto/get-or-create-node graph parent)
                    subscribers (.-subscribers parentnode)]
                (when (contains? subscribers child)
                  (set! (.-subscribers parentnode) (disj subscribers child)))
                (when (satisfies? oldproto/InformedSelector parent)
                  (-apply-selector-effect graph parent [:remove-dep child]))))
            parents))
    retiredeps))

;;;; new api
(defn invalidate-level [graph selectors external-invalids]
  (loop [selectors selectors #_(if (satisfies? IIterable selectors)
                                 selectors
                                 (-iterator selectors))
         newitems (transient #{})
         newdeps (transient [])
         retiredeps (transient [])
         external-invalids external-invalids]
    (if-let [selector (first selectors)]
      (if-let [node (get graph selector)]
        (let [{new-value :value dependencies :dependencies :as vcontainer} (proto/value selector graph (.-state node))
              old-deps (.-refs node)]
          (set! (.-refs node) dependencies)
          ;(prn "dependencies " dependencies)
          ;inlineing invalidate produces error

          (cond
            (= (.-value node) new-value) (do                ;(prn " value-unchanged" (type selector) new-value) ;:value-unchanged
                                           (recur (rest selectors) newitems
                                                  (filtered-set-add newdeps selector dependencies old-deps)
                                                  (filtered-set-add retiredeps selector old-deps dependencies)
                                                  external-invalids))
            (and (.-value node) (instance? hitch.values/NotRealized vcontainer)) (do ; (prn " value-stale" (type selector) (.-value node)) ;:stale
                                                                                   (recur (rest selectors) newitems
                                                                                          (filtered-set-add newdeps selector dependencies old-deps)
                                                                                          (filtered-set-add retiredeps selector old-deps dependencies)
                                                                                          external-invalids))
            (not= (.-value node) new-value) (do
                                              ;(prn " value changed" (type selector) (.-value node) new-value (proto/get-dependents node))
                                              (set! (.-value node) new-value)

                                              ;:value-changed
                                              (recur (rest selectors)
                                                     (if (satisfies? proto/SilentSelector selector)
                                                       newitems
                                                       (reduce conj! newitems (oldproto/get-dependents node)))
                                                     (filtered-set-add newdeps selector dependencies old-deps)
                                                     (filtered-set-add retiredeps selector old-deps dependencies)
                                                     (if (oldproto/-get-external-dependents node)
                                                       (conj! external-invalids selector)
                                                       external-invalids)))))

        (do (prn "Invalidated selector must always be in the graph" selector)
            (recur (rest selectors)
                   newitems
                   newdeps
                   retiredeps
                   external-invalids)))
      (let [ninv (update-dependencies! graph (persistent! newdeps) (persistent! retiredeps))]

        [(persistent! newitems)
         external-invalids]))))

(defn invalidate-selectors [graph selectors]
  ;(prn "invalidate-nodes")
  (loop [[selectors external-invalids] (invalidate-level graph selectors  (transient []))]
    ;(prn "invalidate " selectors)
    (if (not-empty selectors)
      (recur (invalidate-level graph selectors external-invalids))
      (if-let [newinvalids (not-empty (oldproto/take-invalidations! graph))]
        (recur (invalidate-level graph newinvalids external-invalids))
        (persistent! external-invalids)))))

(defn -apply-selector-effect [graph selector effect]
  (let [state-atom (oldproto/get-temp-state graph selector)]
    (swap! state-atom #(proto/command-step selector % effect))))

(defn finalize-effects [graph]
  (let [newstate-map (.-tempstate graph)]
    (set! (.-tempstate graph) {})
    (into [] (comp (map
                     (fn [[selector v]]
                       ;(prn "selector v " selector v)
                       (if-let [node (get graph selector)]
                         (let [{new-state :state :as result} (proto/command-result selector @v)]
                           ;(prn  "new " new-state :recalc-child-selectors (:recalc-child-selectors result) )
                           (set! (.-state node) new-state)
                           (when-let [effect (:effect result)]
                             (when-not @oldproto/scheduled-actions
                               (vreset! oldproto/scheduled-actions true)
                               (schedule-actions graph))
                             (vswap! oldproto/pending-actions conj effect))
                           (if (satisfies? proto/SilentSelector selector)
                             (eduction cat [[selector] (:recalc-child-selectors result) ])
                             [selector]))
                         (prn "node not found"))))
                   cat)
          newstate-map)))

(defn -apply-selector-effect-pairs [graph selector-effect-pairs]
  (run! (fn [[selector effects]]
          (-apply-selector-effect graph selector effects))
        selector-effect-pairs))



(defn normalize-tx! [graph]
  (binding [oldproto/*read-mode* true]
    (loop [invalidations (oldproto/take-invalidations! graph) external-invalids (transient [])]
      ;(prn "invalidations " invalidations (.-tempstate graph))
      (if (not-empty invalidations)
        (recur [] (conj! external-invalids (invalidate-selectors graph invalidations)))
        (if-let [new-invalids (not-empty (finalize-effects graph))]
          (recur new-invalids external-invalids)
          (invalidate-external-items graph (eduction cat (persistent! external-invalids))))))))



(defn process-actions [graph]
  (fn []
    (let [pending-actions @oldproto/pending-actions
          simple-graph (reify ILookup
                         (-lookup [this k]
                           (-lookup this k nil))
                         (-lookup [this k not-found]
                           (if-let [node (get graph k)]
                             (.-value node)
                             not-found)))]
      (vreset! oldproto/scheduled-actions false)
      (vreset! oldproto/pending-actions [])
      (run! (fn [scheduled-action]
              (scheduled-action simple-graph (fn [selector-effect-pairs] (oldproto/apply-commands graph selector-effect-pairs))))
            pending-actions))))

(defn schedule-actions [graph]
  (goog.async.nextTick (process-actions graph)))


;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable tempstate ^:mutable internal-invalidated
                          ^:mutable external-invalidate!]
  ILookup
  (-lookup [o data-selector]
    (-lookup o data-selector nil))
  (-lookup [o data-selector not-found]
    (-lookup nodemap data-selector not-found))
  oldproto/IDependencyGraph
  (create-node! [graph data-selector nf]
    (let [new-node (node data-selector)]
      (when (satisfies? proto/StatefulSelector data-selector)
        (set! tempstate (assoc tempstate data-selector (atom (proto/create data-selector)))))
      (set! nodemap (assoc nodemap data-selector new-node))
      ;next two forms need to be resolved
      (oldproto/-request-invalidations graph data-selector)
      (when-not oldproto/*read-mode*
        (normalize-tx! graph))
      new-node))
  (apply-commands [graph selector-command-pairs]
    (binding [oldproto/*read-mode* true]
      (-apply-selector-effect-pairs graph selector-command-pairs)
      (normalize-tx! graph)))
  (clear-graph! [dgraph]
    (doseq [node (vals nodemap)]
      (oldproto/clear-node! node dgraph))
    (set! nodemap {})
    (set! tempstate  {}))
  (gc [this data-selector]
    #_(do (doseq [d (proto/selector-dependencies data-selector)]
            (proto/undepend! this d))
          true))
  oldproto/IBatching
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
  (DependencyGraph. {}  {} nil identity))

(defn get-node-map [graph]
  (.-nodemap graph))


