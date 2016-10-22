(ns hitch.selector
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.selector-tx-manager :refer []]
            [hitch.mutable.node]))

(defn create-resolved-value
  ([vfn tx-manager]
   (proto/->SelectorValue (vfn tx-manager) (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a]
   (proto/->SelectorValue (vfn tx-manager a) (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b]
   (proto/->SelectorValue (vfn tx-manager a b) (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c]
   (proto/->SelectorValue (vfn tx-manager a b c) (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d]
   (proto/->SelectorValue (vfn tx-manager a b c d) (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d e]
   (proto/->SelectorValue (vfn tx-manager a b c d e) (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d e f]
   (proto/->SelectorValue (vfn tx-manager a b c d e f) (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d e f g]
   (proto/->SelectorValue (vfn tx-manager a b c d e f g) (oldproto/get-depends tx-manager))))

(defn create-unresolved-value [tx-manager]
  (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))

(defn attempt
  ([vfn tx-manager]
   (try
     (create-resolved-value vfn tx-manager)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex)))))
  ([vfn tx-manager a]
   (try
     (create-resolved-value vfn tx-manager a)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex)))))
  ([vfn tx-manager a b]
   (try
     (create-resolved-value vfn tx-manager a b)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex)))))
  ([vfn tx-manager a b c]
   (try
     (create-resolved-value vfn tx-manager a b c)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex)))))
  ([vfn tx-manager a b c d]
   (try
     (create-resolved-value vfn tx-manager a b c d)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex)))))
  ([vfn tx-manager a b c d e]
   (try
     (create-resolved-value vfn tx-manager a b c d e)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex)))))
  ([vfn tx-manager a b c d e f]
   (try
     (create-resolved-value vfn tx-manager a b c d e f)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex)))))
  ([vfn tx-manager a b c d e f g]
   (try
     (create-resolved-value vfn tx-manager a b c d e f g)
     (catch :default ex (if (identical? oldproto/berror ex )
                          (create-unresolved-value tx-manager)
                          (throw ex))))))
