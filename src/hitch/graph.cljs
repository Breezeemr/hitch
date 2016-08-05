(ns hitch.graph
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.protocols :as proto]
            [hitch.graphs.mutable :as mgraph]
            [hitch.nodes.simple :refer [node]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async :as async]))


(def ^:dynamic *execution-mode* true)
(def loaded? proto/loaded?)

(defn get-or-create-node! [graph selector]
  (if-let [n (proto/get-node graph selector)]
    n
    (let [n (proto/add-node! graph selector (if (satisfies? proto/ICreateNode selector)
                                              (proto/-create-node selector graph)
                                              (node selector)))]
      (proto/-recalculate! n graph)
      n)))

(defn try-to-get-node [dependency-graph data-selector]
  (proto/get-node dependency-graph data-selector))

(defn hitch-node
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
(declare apply-effects invalidate-nodes)

(defn hitch
  ([graph selector-constructor]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)
     node
     ))
  ([graph selector-constructor a]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     ;(prn "here"  node  proto/*current-node*)
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)
     node
     ))
  ([graph selector-constructor a b]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)
     node
     ))
  ([graph selector-constructor a b c]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)
     node
     ))
  ([graph selector-constructor a b c d]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)
     node
     ))
  ([graph selector-constructor a b c d f]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)

     node
     ))
  ([graph selector-constructor a b c d f g]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)
     node
     ))
  ([graph selector-constructor a b c d f g h]
   (assert proto/*current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g h)
         [more-effects invalidated] (proto/depend! graph node proto/*current-node*)]
     (apply-effects graph more-effects)
     (invalidate-nodes graph invalidated)
     node
     )))

(defn hook
  ([graph selector-constructor] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor))
  ([graph selector-constructor a] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a))
  ([graph selector-constructor a b] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b))
  ([graph selector-constructor a b c] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c))
  ([graph selector-constructor a b c d] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d))
  ([graph selector-constructor a b c d f] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f))
  ([graph selector-constructor a b c d f g] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g))
  ([graph selector-constructor a b c d f g h] ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g h)))

(defn invalidate-external-items [value ext-items]
  (doseq [subscriber (seq ext-items)
          ;:when (impl/active? subscriber)
          :let [handler (impl/commit subscriber)]]
    (handler value)))
;;;; new api
(defn invalidate-level [graph nodes external-invalids]
  ;(prn "nodes " nodes)
  (loop [nodes nodes
         newitems (transient {})
         external-invalids external-invalids]
    (if-let [node (first nodes)]
      (let [stat (proto/-recalculate! node graph)]          ;inlineing invalidate produces error
        (case stat
          :value-changed (do                                ;(prn  :value-changed)
                           (recur (rest nodes)
                                  (into newitems (proto/get-dependents node))
                                  (assoc! external-invalids (proto/get-value node) (proto/-take-one-time-dependents! node))))
          :value-unchanged (recur (rest nodes) newitems external-invalids)
          :stale (recur (rest nodes) newitems external-invalids)
          ;:transient-error   (recur (rest nodes) newitems external-invalids)
          ;:persistent-error  (recur (rest nodes) newitems external-invalids)
          ))
      [(into #{}
             (mapcat
               proto/get-dependents)
             (persistent! newitems))
       external-invalids])))
(defn invalidate-nodes [graph nodes]
  ;(prn "invalidate-nodes")
  (loop [[nodes external-invalids] (invalidate-level graph nodes (transient {}))]
    (if (not-empty nodes)
      (recur (invalidate-level graph nodes external-invalids))
      (doseq [[value ext-items] (persistent! external-invalids)]
        (invalidate-external-items value ext-items)))))

(defn invalidate-selectors
  ([graph selectors]
   (invalidate-nodes graph (seq (sequence (comp
                                            (map #(proto/get-node graph %))
                                            (remove nil?))
                                          selectors)))))


(defn force-invalidate! [graph node value]
  (let [onetime (proto/-take-one-time-dependents! node)]
    (invalidate-nodes graph (proto/get-dependents node))
    (invalidate-external-items value onetime)))

(defn set-and-invalidate! [graph node value]
  (proto/set-value! node value)
  (force-invalidate! graph node value))

(defn apply-effect-to-node [node effect]

  )

(defn -apply-effects [graph selector-effect-pairs]
  (loop [invalidated-nodes (transient #{}) selector-effect-pairs selector-effect-pairs]
     (if-let [[selector effects] (first selector-effect-pairs)]
       (let [node (get-or-create-node! graph selector)]
         (if (satisfies? proto/SelectorEffects selector)
           (let [old-state (.-state node)
                  [new-state new-selector-effect-pairs] (proto/-apply selector old-state effects)
                  nodes (-apply-effects graph new-selector-effect-pairs)]
                (if (not= old-state new-state)
                  (do
                    (set! (.-state node) new-state)
                    (recur (-> (reduce conj! invalidated-nodes nodes)
                               (conj! node))
                           (rest selector-effect-pairs)))
                  (recur (into invalidated-nodes nodes)
                         (rest selector-effect-pairs))))
             (recur invalidated-nodes (rest selector-effect-pairs))
           ))
       (persistent! invalidated-nodes))))

(defn apply-effects [graph selector-effect-pairs]
  (->> (-apply-effects graph selector-effect-pairs)
       (invalidate-nodes graph)))