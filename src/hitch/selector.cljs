(ns hitch.selector
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.mutable.tx :refer []]
            [cljs.core.async.impl.channels :as impl-chan]
            [cljs.core.async.impl.protocols :as impl]
            [hitch.mutable.node]
            [cljs.core.async :as async]))

(defn handle-selector-value [tx-manager value]
  (let [requests (.-requests tx-manager)]
    (if-some [val (if (satisfies? impl/ReadPort value)
                     (async/poll! value)
                     value)]
      (proto/->SelectorValue val requests)
       (proto/->SelectorUnresolved requests))))

(defn attempt
  ([vfn tx-manager]
   (try
     (let [val (vfn tx-manager)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))))
  ([vfn tx-manager a]
   (try
     (let [val (vfn tx-manager a)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))))
  ([vfn tx-manager a b]
   (try
     (let [val (vfn tx-manager a b)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))))
  ([vfn tx-manager a b c]
   (try
     (let [val (vfn tx-manager a b c)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))))
  ([vfn tx-manager a b c d]
   (try
     (let [val (vfn tx-manager a b c d)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))))
  ([vfn tx-manager a b c d e]
   (try
     (let [val (vfn tx-manager a b c d e)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))))
  ([vfn tx-manager a b c d e f]
   (try
     (let [val (vfn tx-manager a b c d e f)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))))
  ([vfn tx-manager a b c d e f g]
   (try
     (let [val (vfn tx-manager a b c d e f g)]
       (proto/->SelectorValue val (oldproto/get-depends tx-manager)))
     (catch js/Error ex (proto/->SelectorUnresolved (oldproto/get-depends tx-manager))))))
