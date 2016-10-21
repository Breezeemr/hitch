(ns hitch.mutable.graph
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.mutable.node :as simple :refer [node NODE-NOT-RESOLVED-SENTINEL]]))

(def ^:dynamic *read-mode* false)
(def pending-actions (volatile! []))
(def scheduled-actions (volatile! false))

(defn get-or-create-node [graph data-selector]
  (if-let [n (get (.-nodemap graph) data-selector)]
    n
    (do                                                     ;(prn "get-or-create-node" )
      (oldproto/create-node! graph data-selector nil)
      (get (.-nodemap graph) data-selector))))



(defn get-temp-state [graph selector]
  (assert (satisfies? proto/CommandableSelector selector) )
  (if-let [ts (get (.-tempstate graph) selector)]
    ts
    (let [node (get-or-create-node graph selector)
          ts (atom (proto/command-accumulator selector (.-state node)))]
      (set! (.-tempstate graph) (assoc (.-tempstate graph) selector ts))
      ts)))

(declare   schedule-actions)                                  ;invalidate-nodes normalize-tx!
(defn invalidate-external-items [graph ext-items]
  (run! (fn [changed-selector]
          (run! (fn [external-dep]
                  (oldproto/-change-notify external-dep))
                (oldproto/-get-external-dependents graph changed-selector)))
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
              (let [parentnode (get-or-create-node graph parent)
                    subscribers (.-subscribers parentnode)]
                (when-not (contains? subscribers child)
                  ;(prn "add subscrption" child "to " parent)
                  (set! (.-subscribers parentnode) (conj subscribers child)))
                (when (satisfies? proto/InformedSelector parent)
                  (-apply-selector-effect graph parent [:add-dep child]))
                ))
            parents))
    newdeps)
  (run!
    (fn [[child parents]]
      (run! (fn [parent]
              (let [parentnode (get-or-create-node graph parent)
                    subscribers (.-subscribers parentnode)]
                (when (contains? subscribers child)
                  (set! (.-subscribers parentnode) (disj subscribers child)))
                (when (satisfies? proto/InformedSelector parent)
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
      (if-let [node (get (.-nodemap graph) selector)]
        (let [{new-value :value dependencies :parents :as vcontainer} (proto/value selector graph (.-state node))
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
            (and (.-value node) (instance? proto/SelectorUnresolved vcontainer)) (do ; (prn " value-stale" (type selector) (.-value node)) ;:stale
                                                                                   (recur (rest selectors) newitems
                                                                                          (filtered-set-add newdeps selector dependencies old-deps)
                                                                                          (filtered-set-add retiredeps selector old-deps dependencies)
                                                                                          external-invalids))
            (not= (.-value node) new-value) (do
                                              ;(prn " value changed" vcontainer(type selector) (.-value node) new-value )
                                              (set! (.-value node) new-value)

                                              ;:value-changed
                                              (recur (rest selectors)
                                                     (if (satisfies? proto/SilentSelector selector)
                                                       newitems
                                                       (reduce conj! newitems (.-subscribers node)))
                                                     (filtered-set-add newdeps selector dependencies old-deps)
                                                     (filtered-set-add retiredeps selector old-deps dependencies)
                                                     (if (oldproto/-get-external-dependents graph selector)
                                                       (conj! external-invalids selector)
                                                       external-invalids)))))

        (do                                                 ;(prn "Invalidated selector must always be in the graph" selector)
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
  (let [state-atom (get-temp-state graph selector)]
    (swap! state-atom #(proto/command-step selector % effect))))

(defn finalize-effects [graph]
  (let [newstate-map (.-tempstate graph)]
    (set! (.-tempstate graph) {})
    (into [] (comp (map
                     (fn [[selector v]]
                       ;(prn "selector v " selector v)
                       (if-let [node (get (.-nodemap graph) selector)]
                         (let [{new-state :state :as result} (proto/command-result selector @v)]
                           ;(prn  "new " new-state :recalc-child-selectors (:recalc-child-selectors result) )
                           (set! (.-state node) new-state)
                           (when-let [effect (:effect result)]
                             (when-not @scheduled-actions
                               (vreset! scheduled-actions true)
                               (schedule-actions graph))
                             (vswap! pending-actions conj effect))
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
  (binding [*read-mode* true]
    (loop [invalidations (oldproto/take-invalidations! graph) external-invalids (transient [])]
      ;(prn "invalidations " invalidations (.-tempstate graph))
      (if (not-empty invalidations)
        (recur [] (conj! external-invalids (invalidate-selectors graph invalidations)))
        (if-let [new-invalids (not-empty (finalize-effects graph))]
          (recur new-invalids external-invalids)
          (invalidate-external-items graph (eduction cat (persistent! external-invalids))))))))



(defn process-actions [graph]
  (fn []
    (let [current-pending-actions @pending-actions
          simple-graph (reify ILookup
                         (-lookup [this k]
                           (-lookup this k nil))
                         (-lookup [this k not-found]
                           (if-let [node (get (.-nodemap graph) k)]
                             (.-value node)
                             not-found)))]
      (vreset! scheduled-actions false)
      (vreset! pending-actions [])
      (run! (fn [scheduled-action]
              (scheduled-action simple-graph (fn [selector-effect-pairs] (oldproto/apply-commands graph selector-effect-pairs))))
            current-pending-actions))))

(defn schedule-actions [graph]
  (goog.async.nextTick (process-actions graph)))


;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable tempstate ^:mutable internal-invalidated
                          ^:mutable external-invalidate!]
  ILookup
  (-lookup [o data-selector]
    (-lookup o data-selector nil))
  (-lookup [o data-selector not-found]
    (let [n (get nodemap data-selector not-found)]
      ;(prn n)
      (if (identical? n not-found)
        not-found
        (let [val (.-value n)]
          (if (identical? val NODE-NOT-RESOLVED-SENTINEL)
            not-found
            val)))))
  oldproto/IDependencyGraph
  (create-node! [graph data-selector nf]
    ;(prn "create " data-selector)
    (let [n (get nodemap data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
      (if (identical? n oldproto/NOT-IN-GRAPH-SENTINEL)
        (let [new-node (node data-selector)]
          (when (satisfies? proto/StatefulSelector data-selector)
            (set! tempstate (assoc tempstate data-selector (atom (.-state new-node)))))
          (set! nodemap (assoc nodemap data-selector new-node))
          ;next two forms need to be resolved
          (oldproto/-request-invalidations graph data-selector)
          (when-not *read-mode*
            (normalize-tx! graph))
          (let [v (.-value new-node)]
            (if (identical? v NODE-NOT-RESOLVED-SENTINEL)
              nf
              v)))
        (let [v (.-value n)]
          (if (identical? v NODE-NOT-RESOLVED-SENTINEL)
            nf
            v)))))
  (apply-commands [graph selector-command-pairs]
    (binding [*read-mode* true]
      (-apply-selector-effect-pairs graph selector-command-pairs)
      (normalize-tx! graph)))
  (update-parents [this child adds rms ]
    (doseq [add adds
            :let [n (get nodemap add)]
            :when n]
      (set! (.-external-dependencies n) ((fnil conj #{}) (.-external-dependencies n) child)))
    (doseq [rm rms
            :let [n (get nodemap rm)]
            :when n]
      (set! (.-external-dependencies n) (not-empty (disj (.-external-dependencies n) child)))))
  (-get-external-dependents [this parent]
    (when-let [n (get nodemap parent)]
      ;(prn "Exdep" (.-external-dependencies n))
      (.-external-dependencies n)
      ))
  (clear-graph! [dgraph]
    (doseq [node (vals nodemap)]
      (simple/clear-node! node dgraph))
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


