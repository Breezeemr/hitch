(ns hitch.selector
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.oldprotocols :as proto]
            [hitch.dependent-transaction :refer []]
            [hitch.values :refer [->Realized ->NotRealized]]
            [cljs.core.async.impl.channels :as impl-chan]
            [cljs.core.async.impl.protocols :as impl]
            [hitch.nodes.simple]
            [cljs.core.async :as async]))

(defn handle-selector-value [tx value]
  ;(prn "handle-selector-value")
  (let [graph (.-graph tx)
        child-selector (.-target tx)
        requests (.-requests tx)
        child-node (proto/peek-node graph child-selector)]
    (if-some [val (if (satisfies? impl/ReadPort value)
                     (async/poll! value)
                     value)]
      (->Realized val requests)
       (->NotRealized requests))))
