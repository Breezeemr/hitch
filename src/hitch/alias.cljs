(ns hitch.alias
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]))

(defrecord AliasSelector [k]
  proto/ISelectorSingleton
  proto/ISelector
  (selector-init [this _])
  (selector-invoke [this alias _]
    (if alias
      (proto/get-value alias)
      :hitch/not-loaded)))

(defn alias [k]
  (->AliasSelector k))

(defn set-alias
  ([k data-selector]
   (set-alias graph/*default-graph* k data-selector))
  ([graph k data-selector]
   (when data-selector
     (let [alias-node (graph/get-or-create-node graph (->AliasSelector k))
           data-selector-node (graph/get-or-create-node graph data-selector)]
       (set! (.-refs alias-node) data-selector-node)
       (proto/depend! data-selector-node alias-node )
       (proto/invalidate! alias-node data-selector-node)
       ))))

(defn get-alias
  ([k]
   (get-alias graph/*default-graph* k))
  ([graph k]
   (graph/getn (->AliasSelector k))))

