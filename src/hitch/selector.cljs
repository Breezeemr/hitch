(ns hitch.selector
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.selector-tx-manager :refer []]
            [hitch.mutable.node]))

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
