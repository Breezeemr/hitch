(ns hitch.graph
  (:require [hitch.protocols :as proto]
            [hitch.graphs.mutable :as mgraph]
            [cljs.core.async.impl.protocols :as impl]
            [hitch.eager-go :include-macros true]
            [cljs.core.async :as async]))

(def ^:dynamic *current-node* nil)
(def ^:dynamic *execution-mode* true)
(def loaded? proto/loaded?)

(defn get-or-create-node! [graph selector]
  (if-let [n (proto/get-node graph selector)]
    n
    (let [n (proto/add-node! graph selector (proto/-create-node selector graph))]
      (proto/-recalculate! n graph)
      n)))

(defn try-to-get-node [dependency-graph data-selector]
  (proto/get-node dependency-graph data-selector))

(defn hitch-node
  ([graph selector-constructor] (get-or-create-node! graph (proto/-selector selector-constructor graph)))
  ([graph selector-constructor a] (get-or-create-node! graph (proto/-selector selector-constructor graph a)))
  ([graph selector-constructor a b] (get-or-create-node! graph (proto/-selector selector-constructor graph a b)))
  ([graph selector-constructor a b c] (get-or-create-node! graph (proto/-selector selector-constructor graph a b c)))
  ([graph selector-constructor a b c d] (get-or-create-node! graph (proto/-selector selector-constructor graph a b c d)))
  ([graph selector-constructor a b c d f] (get-or-create-node! graph (proto/-selector selector-constructor graph a b c d f)))
  ([graph selector-constructor a b c d f g] (get-or-create-node! graph (proto/-selector selector-constructor graph a b c d f g)))
  ([graph selector-constructor a b c d f g h] (get-or-create-node! graph (proto/-selector selector-constructor graph a b c d f g h))))

(defn hitch-eval
  ([graph selector-constructor] (proto/-eval selector-constructor graph))
  ([graph selector-constructor a] (proto/-eval selector-constructor graph a))
  ([graph selector-constructor a b] (proto/-eval selector-constructor graph a b))
  ([graph selector-constructor a b c] (proto/-eval selector-constructor graph a b c))
  ([graph selector-constructor a b c d] (proto/-eval selector-constructor graph a b c d))
  ([graph selector-constructor a b c d f] (proto/-eval selector-constructor graph a b c d f))
  ([graph selector-constructor a b c d f g] (proto/-eval selector-constructor graph a b c d f g))
  ([graph selector-constructor a b c d f g h] (proto/-eval selector-constructor graph a b c d f g h)))

(defn hitch
  ([graph selector-constructor]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor)]
     (proto/depend! graph node *current-node*)
     node
     ))
  ([graph selector-constructor a]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a)]
     ;(prn "here"  node  *current-node*)
     (proto/depend! graph node *current-node*)
     node
     ))
  ([graph selector-constructor a b]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b)]
     (proto/depend! graph node *current-node*)
     node
     ))
  ([graph selector-constructor a b c]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c)]
     (proto/depend! graph node *current-node*)
     node
     ))
  ([graph selector-constructor a b c d]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d)]
     (proto/depend! graph node *current-node*)
     node
     ))
  ([graph selector-constructor a b c d f]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f)]
     (proto/depend! graph node *current-node*)
     node
     ))
  ([graph selector-constructor a b c d f g]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g)]
     (proto/depend! graph node *current-node*)
     node
     ))
  ([graph selector-constructor a b c d f g h]
   (assert *current-node*)
   (let [node ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g h)]
     (proto/depend! graph node *current-node*)
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
      (let [exts (persistent! external-invalids)]
        (doseq [[value ext-items] exts]
          (invalidate-external-items value ext-items))))))

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

(defn resolve-all [nodes]
  (hitch.eager-go/eager-go
    (loop [results (empty nodes) nodes (doall nodes)]
      (if-let [node (first nodes)]
        (recur (conj results (async/<! node)) (rest nodes))
        results))))