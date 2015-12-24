(ns hitch.graphs.mutable
  (:require [hitch.protocols   :as proto ]))


;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable gc-list ^:mutable external-invalidate!]
  proto/IDependencyGraph
  (get-node [this data-selector]
    (get nodemap data-selector))
  (add-node! [this data-selector new-node]
    (set! nodemap (assoc nodemap data-selector new-node)))
  (clear-graph! [dgraph]
    (doseq [node (vals nodemap)]
      (proto/clear-node! node dgraph ))
    (set! nodemap {})
    (set! gc-list []))
  (gc [this data-selector]
    #_(do (doseq [d (proto/selector-dependencies data-selector)]
            (proto/undepend! this d))
          true))
  (invalidate-selectors! [graph selectors]

    )
  proto/IEvaluateRequest
  (eval-request [this parent-node selector-constructor]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor)
                (selector-constructor))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node)))
  (eval-request [this parent-node selector-constructor a]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor a)
                (selector-constructor a))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node)))
  (eval-request [this parent-node selector-constructor a b]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor a b)
                (selector-constructor a b))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node)))
  (eval-request [this parent-node selector-constructor a b c]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor a b c)
                (selector-constructor a b c))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node)))
  (eval-request [this parent-node selector-constructor a b c d]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor a b c d)
                (selector-constructor a b c d))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node)))
  (eval-request [this parent-node selector-constructor a b c d f]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor a b c d f)
                (selector-constructor a b c d f))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node)))
  (eval-request [this parent-node selector-constructor a b c d f g]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor a b c d f g)
                (selector-constructor a b c d f g))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node)))
  (eval-request [this parent-node selector-constructor a b c d f g h]
    (let [sel (if (satisfies? proto/ISelectorConstructor selector-constructor)
                (proto/construct-selector selector-constructor a b c d f g h)
                (selector-constructor a b c d f g h))
          node (proto/get-or-create-node this sel parent-node nil)]
      (proto/get-value node))))

(defn graph []
  (DependencyGraph. {} [] identity))

(defn get-node-map [graph]
  (.-nodemap graph))


