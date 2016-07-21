(ns hitch.selectors.reference
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph])
  (:require-macros [hitch.selector :refer [def-selector]]
                   [hitch.eager-go :refer [eager-go]]))

(def-selector reference [graph k]
              (when-let [selector (and graph/*current-node* (.-refs graph/*current-node*))]
                (<! (graph/get-or-create-node! graph selector))))

(defn set-reference!
  ([graph k data-selector]
   (when data-selector
     (let [kselector (proto/-selector reference graph k)
           reference-node (graph/try-to-get-node graph kselector)
           data-selector-node (graph/get-or-create-node! graph data-selector)]
       (when reference-node
         (when (.-refs reference-node)
           (proto/undepend! graph reference-node data-selector-node))
         (set! (.-refs reference-node) data-selector-node)
         (proto/depend! graph reference-node data-selector-node)
         (graph/invalidate-nodes graph [reference-node]))
       ))))

