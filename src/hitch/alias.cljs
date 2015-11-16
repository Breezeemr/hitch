(ns hitch.alias
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]))

(defrecord AliasSelector [k]
  proto/ISelectorSingleton
  proto/ISelector
  (selector-init [this _])
  (selector-ready? [this _ _] true)
  (selector-invoke [this alias _]
    (if alias
      (proto/get-value alias)
      :hitch/not-loaded)))

(defn alias [k]
  (->AliasSelector k))

(defn set-alias
  ([k data-selector]
   (when data-selector
     (let [alias-node (graph/hitch-node (->AliasSelector k))
           data-selector-node (graph/hitch-node data-selector)]
       (set! (.-refs alias-node) data-selector-node)
       (proto/depend! data-selector-node alias-node )
       (proto/invalidate! alias-node data-selector-node)
       ))))

(defn get-alias
  ([k]
   (get-alias graph/*default-graph* k))
  ([graph k]
   (graph/getn (->AliasSelector k))))

