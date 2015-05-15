(ns hitch.alias
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]))

(defrecord AliasSelector [k]
  proto/IDataSelectorStaticRefs
  (selector-dependencies [_] [])
  proto/IDataSelector
  (get-value! [this args]
    (prn "evaluate alias" k args)
    (if (not-empty args)
      (first args)
      :hitch.graph/not-loaded)))

(defn alias [k]
  (->AliasSelector k))

(defn set-alias
  ([k data-selector]
   (set-alias graph/*default-graph* k data-selector))
  ([graph k data-selector]
   (when data-selector
     (let [alias-node (proto/get-node graph (->AliasSelector k))
           data-selector-node (proto/get-node graph data-selector)]
       (prn "set alias" k data-selector [data-selector-node])
       (set! (.-refs alias-node) [data-selector-node])
       (proto/depend! data-selector-node alias-node )
       (proto/invalidate! alias-node)
       ))))

(defn get-alias
  ([k]
   (get-alias graph/*default-graph* k))
  ([graph k]
   (graph/getn (->AliasSelector k))))

