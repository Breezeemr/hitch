(ns hitch.selectors.mutable-var
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]))

(defselector mutable-var [graph k]
             (proto/get-value graph/*current-node*))

(defn set-var!
  ([graph k value]
   (let [kselector (proto/-selector mutable-var graph k)
         var-node (graph/get-or-create-node! graph kselector)]
     (graph/set-and-invalidate! graph var-node value)
     )))
