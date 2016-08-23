(ns hitch.graph
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.protocols :as proto]
            [hitch.graphs.mutable :as mgraph]
            [hitch.nodes.simple :refer [node]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async :as async]))


(def ^:dynamic *execution-mode* true)
(declare apply-effects invalidate-nodes normalize-tx!)

(defn get-or-create-node! [graph selector]
  (let [n (binding [proto/*read-mode* true] (proto/get-or-create-node graph selector))]
    (when (not proto/*read-mode*)
      (normalize-tx! graph (proto/take-effects! graph)
                     (proto/take-invalidations! graph)))
    n))

(defn hitch-sel [graph selector]
  (let [n (binding [proto/*read-mode* true] (proto/subscribe-node graph selector))]
    (when (not proto/*read-mode*)
      (normalize-tx! graph (proto/take-effects! graph)
                     (proto/take-invalidations! graph)))
    n))

(defn try-to-get-node [dependency-graph data-selector]
  (proto/peek-node dependency-graph data-selector))

(defn hitch-node
  ([graph selector-constructor] (hitch-sel graph (proto/-selector selector-constructor)))
  ([graph selector-constructor a] (hitch-sel graph (proto/-selector selector-constructor a)))
  ([graph selector-constructor a b] (hitch-sel graph (proto/-selector selector-constructor a b)))
  ([graph selector-constructor a b c] (hitch-sel graph (proto/-selector selector-constructor a b c)))
  ([graph selector-constructor a b c d] (hitch-sel graph (proto/-selector selector-constructor a b c d)))
  ([graph selector-constructor a b c d f] (hitch-sel graph (proto/-selector selector-constructor a b c d f)))
  ([graph selector-constructor a b c d f g] (hitch-sel graph (proto/-selector selector-constructor a b c d f g)))
  ([graph selector-constructor a b c d f g h] (hitch-sel graph (proto/-selector selector-constructor a b c d f g h))))

(defn hook-node
  ([graph selector-constructor] (get-or-create-node! graph (proto/-selector selector-constructor)))
  ([graph selector-constructor a] (get-or-create-node! graph (proto/-selector selector-constructor a)))
  ([graph selector-constructor a b] (get-or-create-node! graph (proto/-selector selector-constructor a b)))
  ([graph selector-constructor a b c] (get-or-create-node! graph (proto/-selector selector-constructor a b c)))
  ([graph selector-constructor a b c d] (get-or-create-node! graph (proto/-selector selector-constructor a b c d)))
  ([graph selector-constructor a b c d f] (get-or-create-node! graph (proto/-selector selector-constructor a b c d f)))
  ([graph selector-constructor a b c d f g] (get-or-create-node! graph (proto/-selector selector-constructor a b c d f g)))
  ([graph selector-constructor a b c d f g h] (get-or-create-node! graph (proto/-selector selector-constructor a b c d f g h))))

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

(defn invalidate-external-items [value ext-items]
  (doseq [subscriber (seq ext-items)
          ;:when (impl/active? subscriber)
          :let [handler (impl/commit subscriber)]]
    (handler value)))
;;;; new api
(defn invalidate-level [graph nodes external-invalids]
  (loop [nodes nodes
         newitems (transient #{})
         external-invalids external-invalids]
    (if-let [node (first nodes)]
      (let [stat (proto/-recalculate! node graph)]          ;inlineing invalidate produces error
        (case stat
          :value-changed (do                                ;(prn  :value-changed)
                           (recur (rest nodes)
                                  (reduce conj! newitems  (proto/get-dependents node))
                                  (conj! external-invalids [(proto/get-value node) (proto/-take-one-time-dependents! node)])))
          :value-unchanged (recur (rest nodes) newitems external-invalids)
          :stale (recur (rest nodes) newitems external-invalids)
          ;:transient-error   (recur (rest nodes) newitems external-invalids)
          ;:persistent-error  (recur (rest nodes) newitems external-invalids)
          ))
      [(persistent! newitems)
       external-invalids])))
(defn invalidate-nodes [graph nodes]
  ;(prn "invalidate-nodes")
  (loop [[nodes external-invalids] (invalidate-level graph nodes (transient []))]
    (if (not-empty nodes)
      (recur (invalidate-level graph nodes external-invalids))
      (doseq [[value ext-items] (persistent! external-invalids)]
        (invalidate-external-items value ext-items)))))

(defn invalidate-selectors
  ([graph selectors]
   (invalidate-nodes graph (seq (sequence (comp
                                            (map #(proto/peek-node graph %))
                                            (remove nil?))
                                          selectors)))))

(def mcident (mapcat identity))
(defn educat [items]
  (eduction mcident items))

(defn -apply-effects [graph selector-effect-pairs]
  (loop [invalidated-nodes (transient #{}) selector-effect-pairs selector-effect-pairs selector-effect-pairs-acc (transient [])]
     (if-let [[selector effects] (first selector-effect-pairs)]
       (let [node (proto/peek-node graph selector)]
         (assert node "you cannot apply effects to a selector that is not depended on")
         (if (satisfies? proto/SelectorEffects selector)
           (let [old-state (.-state node)
                  [new-state  new-selector-effect-pairs nodes] (proto/-apply selector old-state effects)]
                (if (not= old-state new-state)
                  (do   #_(prn "old " old-state "new " new-state)
                    (set! (.-state node) new-state)
                    (recur (-> (reduce conj! invalidated-nodes nodes)
                               (conj! node))
                           (rest selector-effect-pairs)
                           (conj! selector-effect-pairs-acc new-selector-effect-pairs)))
                  (recur (into invalidated-nodes nodes)
                         (rest selector-effect-pairs)
                         (conj! selector-effect-pairs-acc  new-selector-effect-pairs))))
             (recur invalidated-nodes (rest selector-effect-pairs) selector-effect-pairs-acc)))
       (let [persistent-selector-effect-pairs-acc (persistent! selector-effect-pairs-acc)]
         (if (not-empty persistent-selector-effect-pairs-acc)
           (recur invalidated-nodes (educat persistent-selector-effect-pairs-acc) (transient []))
           (persistent! invalidated-nodes))))))



(defn normalize-tx! [graph effects invalidations]
  (binding [proto/*read-mode* true]
    (loop [stage :effects effects effects invalidations invalidations]
      (when (or (not-empty effects) (not-empty invalidations))
        ;(prn stage (count effects) (count invalidations))
        ;(prn :effects effects)
        ;(prn :invaldations invalidations)
        (case stage
          :effects (recur :invalidate
                          (proto/take-effects! graph)
                          (educat [invalidations
                                   (-apply-effects graph effects)
                                   (proto/take-invalidations! graph)]))
          :invalidate (do (invalidate-nodes graph invalidations)
                        (recur :effects (educat [effects (proto/take-effects! graph)]) (proto/take-invalidations! graph))))))))

(defn apply-effects [graph selector-effect-pairs]
  (normalize-tx! graph selector-effect-pairs #{}))