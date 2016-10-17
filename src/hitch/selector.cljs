(ns hitch.selector
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.mutable.tx :refer []]
            [cljs.core.async.impl.channels :as impl-chan]
            [cljs.core.async.impl.protocols :as impl]
            [hitch.mutable.node]
            [cljs.core.async :as async]))

(defn handle-selector-value [tx value]
  ;(prn "handle-selector-value")
  (let [graph (.-graph tx)
        child-selector (.-target tx)
        requests (.-requests tx)
        child-node (get graph child-selector)]
    (if-some [val (if (satisfies? impl/ReadPort value)
                     (async/poll! value)
                     value)]
      (proto/->SelectorValue val requests)
       (proto/->SelectorUnresolved requests))))
