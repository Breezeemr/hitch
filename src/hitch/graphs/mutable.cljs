(ns hitch.graphs.mutable
  (:require [hitch.protocols :as proto]
            [hitch.nodes.simple :refer [node]]))

;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable gc-list ^:mutable internal-invalidated
                          ^:mutable effects ^:mutable external-invalidate!]
  proto/IDependencyGraph
  (peek-node [this data-selector]
    (get nodemap data-selector))
  (create-node! [this data-selector]
    (let [new-node (node data-selector)]
      (when (satisfies? proto/StatefulSelector data-selector)
        (set! (.-state new-node) (proto/init data-selector)))
      (set! nodemap (assoc nodemap data-selector new-node))
      new-node))
  (clear-graph! [dgraph]
    (doseq [node (vals nodemap)]
      (proto/clear-node! node dgraph))
    (set! nodemap {})
    (set! gc-list []))
  (gc [this data-selector]
    #_(do (doseq [d (proto/selector-dependencies data-selector)]
            (proto/undepend! this d))
          true))
  proto/IBatching
  (-request-effects [graph effect]
    (when effect
      (set! effects (conj! effects effect))))
  (-request-invalidations [graph invalidaitons]
    (when invalidaitons
      (set! internal-invalidated (conj! internal-invalidated invalidaitons))))
  (take-effects! [graph]
    (let [ret effects]
      (set! effects (transient []))
      (eduction (mapcat identity) (persistent! ret))
      ))
  (take-invalidations! [graph]
    (let [ret internal-invalidated]
      (set! internal-invalidated (transient []))
      (eduction (mapcat identity) (persistent! ret)))))

(defn graph []
  (DependencyGraph. {} [] (transient []) (transient []) identity))

(defn get-node-map [graph]
  (.-nodemap graph))


