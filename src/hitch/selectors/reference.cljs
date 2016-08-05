(ns hitch.selectors.reference
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]))

(defselector reference [graph k]
             (when-let [selector (and proto/*current-node* (.-refs proto/*current-node*))]
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

