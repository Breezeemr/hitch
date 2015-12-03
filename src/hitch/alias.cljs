(ns hitch.alias
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]))

(defrecord AliasSelector [k]
  proto/ISelectorSingleton
  proto/ISelector
  (selector-init [this graph _])
  (selector-ready? [this graph _ ] true)
  (selector-invoke [this graph current-node]
    (.-value current-node)))

(defn alias [k]
  (->AliasSelector k))

(defn set-alias
  ([graph node k data-selector]
   (when data-selector
     (let [kselector (->AliasSelector k)
           alias-node (graph/get-node graph  kselector)
           data-selector-node (graph/get-node graph  data-selector)]
       (proto/set-value! alias-node data-selector-node)
       (proto/alias! data-selector-node alias-node )
       (graph/invalidate-selectors graph [kselector])
       ))))

(defn get-alias
  ([graph node k]
   (proto/eval-request graph node ->AliasSelector k)))

