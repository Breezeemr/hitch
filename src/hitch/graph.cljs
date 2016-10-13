(ns hitch.graph
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graphs.mutable :as mgraph]
            [hitch.nodes.simple :refer [node]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async :as async]))


(def ^:dynamic *execution-mode* true)
(declare apply-effects invalidate-nodes normalize-tx! schedule-actions)

(defn get-or-create-node! [graph selector]
  (let [n (binding [oldproto/*read-mode* true] (oldproto/get-or-create-node graph selector))]
    (when (not oldproto/*read-mode*)
      (normalize-tx! graph))
    n))
(defn make-hook [graph selector]
  (let [n (get-or-create-node! graph selector)
        h  (oldproto/mkhook n)]
    (oldproto/-add-external-dependent n h)
    h
    #_(if-some [val (.-value n)]
        (async/put! prom val)
        (let [h (oldproto/->Hook n nil)]
          (prn "made hook")
          (oldproto/-add-external-dependent n h)))
    ;prom
    ))

(defn hitch-sel [graph selector]
  (let [n (binding [oldproto/*read-mode* true] (oldproto/subscribe-node graph selector))]
    (when (not oldproto/*read-mode*)
      (normalize-tx! graph))
    n))

(defn try-to-get-node [dependency-graph data-selector]
  (get dependency-graph data-selector))

(defn hitch-node
  ([graph selector-constructor] (hitch-sel graph (oldproto/-selector selector-constructor)))
  ([graph selector-constructor a] (hitch-sel graph (oldproto/-selector selector-constructor a)))
  ([graph selector-constructor a b] (hitch-sel graph (oldproto/-selector selector-constructor a b)))
  ([graph selector-constructor a b c] (hitch-sel graph (oldproto/-selector selector-constructor a b c)))
  ([graph selector-constructor a b c d] (hitch-sel graph (oldproto/-selector selector-constructor a b c d)))
  ([graph selector-constructor a b c d f] (hitch-sel graph (oldproto/-selector selector-constructor a b c d f)))
  ([graph selector-constructor a b c d f g] (hitch-sel graph (oldproto/-selector selector-constructor a b c d f g)))
  ([graph selector-constructor a b c d f g h] (hitch-sel graph (oldproto/-selector selector-constructor a b c d f g h))))

(defn hook-node
  ([graph selector-constructor] (make-hook graph (oldproto/-selector selector-constructor)))
  ([graph selector-constructor a] (make-hook graph (oldproto/-selector selector-constructor a)))
  ([graph selector-constructor a b] (make-hook graph (oldproto/-selector selector-constructor a b)))
  ([graph selector-constructor a b c] (make-hook graph (oldproto/-selector selector-constructor a b c)))
  ([graph selector-constructor a b c d] (make-hook graph (oldproto/-selector selector-constructor a b c d)))
  ([graph selector-constructor a b c d f] (make-hook graph (oldproto/-selector selector-constructor a b c d f)))
  ([graph selector-constructor a b c d f g] (make-hook graph (oldproto/-selector selector-constructor a b c d f g)))
  ([graph selector-constructor a b c d f g h] (make-hook graph (oldproto/-selector selector-constructor a b c d f g h))))

(defn hitch-eval
  ([graph selector-constructor] (selector-constructor graph))
  ([graph selector-constructor a] (selector-constructor graph a))
  ([graph selector-constructor a b] (selector-constructor graph a b))
  ([graph selector-constructor a b c] (selector-constructor graph a b c))
  ([graph selector-constructor a b c d] (selector-constructor graph a b c d))
  ([graph selector-constructor a b c d f] (selector-constructor graph a b c d f))
  ([graph selector-constructor a b c d f g] (selector-constructor graph a b c d f g))
  ([graph selector-constructor a b c d f g h] (selector-constructor graph a b c d f g h)))



(defn hitch
  ([graph selector-constructor]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor))
  ([graph selector-constructor a]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a))
  ([graph selector-constructor a b]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b))
  ([graph selector-constructor a b c]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c))
  ([graph selector-constructor a b c d]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d))
  ([graph selector-constructor a b c d f]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f))
  ([graph selector-constructor a b c d f g]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g))
  ([graph selector-constructor a b c d f g h]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g h)))

(defn hook
  ([graph selector-constructor] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor))
  ([graph selector-constructor a] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a))
  ([graph selector-constructor a b] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b))
  ([graph selector-constructor a b c] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c))
  ([graph selector-constructor a b c d] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d))
  ([graph selector-constructor a b c d f] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d f))
  ([graph selector-constructor a b c d f g] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d f g))
  ([graph selector-constructor a b c d f g h] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d f g h)))

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
                            (when (instance? oldproto/EffectResultAction result)
                              (when-not @oldproto/scheduled-actions
                                (vreset! oldproto/scheduled-actions true)
                                (schedule-actions graph))
                              (vswap! oldproto/pending-actions conj (:action result)))
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

(defn apply-effects [graph selector-effect-pairs]
 (binding [oldproto/*read-mode* true]
     (-apply-selector-effect-pairs graph selector-effect-pairs)
     (normalize-tx! graph)))

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
              (scheduled-action simple-graph (fn [selector-effect-pairs] (apply-effects graph selector-effect-pairs))))
            pending-actions))))

(defn schedule-actions [graph]
  (goog.async.nextTick (process-actions graph)))