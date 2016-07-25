(ns hitch.selectors.kv-store
  (:refer-clojure :exclude [key])
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]

            [hitch.nodes.simple :as node]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]])
  )

(defrecord KVStoreServiceSelector [keyspace]
  proto/ICreateNode
  (-create-node [this graph]
    (let [store (atom {})
          node (node/node this store)]
      (add-watch store :invalidate
                 (fn [key atom old-state new-state]
                   ;(prn "watch " key atom old-state new-state)
                   (when (not= old-state new-state)
                     (graph/force-invalidate! graph node new-state))))
      node))
  IFn
  (-invoke [this graph]
    (proto/get-value graph/*current-node*)))

(def keyspace
  (reify
    proto/ISelectorFactory
    (-eval [this graph ks]
      (assert nil "alias is stateful and should not be evaled")
      )
    (-selector [this graph ks]
      (assert graph)
      (->KVStoreServiceSelector ks))))

(defselector key [graph kspace k]
             (let [kvstore (<! (graph/hitch graph keyspace kspace))]
                (get @kvstore k)))