(ns hitch.graphs.mutable
  (:require [hitch.protocols :as proto]))


;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable gc-list ^:mutable internal-invalidated
                          ^:mutable external-invalidated ^:mutable external-invalidate!]
  proto/IDependencyGraph
  (get-node [this data-selector]
    (get nodemap data-selector))
  (add-node! [this data-selector new-node]
    (set! nodemap (assoc nodemap data-selector new-node))
    new-node)
  (clear-graph! [dgraph]
    (doseq [node (vals nodemap)]
      (proto/clear-node! node dgraph))
    (set! nodemap {})
    (set! gc-list []))
  (gc [this data-selector]
    #_(do (doseq [d (proto/selector-dependencies data-selector)]
            (proto/undepend! this d))
          true))
  (invalidate-selectors! [graph selectors]

    )
  proto/IDependencyTracker
  (depend! [graph dependee dependent]
    (proto/node-depend! dependee dependent))
  (undepend! [graph dependee dependent]
    (proto/node-undepend! dependee dependent)))

(defn graph []
  (DependencyGraph. {} [] #{} #{} identity))

(defn get-node-map [graph]
  (.-nodemap graph))


