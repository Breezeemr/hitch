(ns hitch.mutable.node
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.mutable.tx :as dtx]
            [cljs.core.async :as async]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as imp-chan]))


(defonce NODE-NOT-RESOLVED-SENTINEL
         (reify Object
           (toString [this] "NODE-NOT-RESOLVED-SENTINEL")
           IPrintWithWriter
           (-pr-writer [_ writer opts]
             (-write writer "#NODE-NOT-RESOLVED-SENTINEL"))))

(defprotocol IDependencyNode
  "A utility API for tracking dependencies, allows us to provide more
   advanced options for assembling tracker policies"
  (-get-value [this]
              "Returns cached value if exists for params")
  (set-value! [this new-value]
              "Informs store that a particular params yeilds value given current store + deps")
  (-dependents [this]
               "The current dependencies encountered by this tracker")
  (-data-selector [this]
                  "The nodes that return this nodes value")
  (clear-node! [this graph]))

(deftype Node [selector ^:mutable value ^:mutable state ^:mutable stale? ^:mutable subscribers ^:mutable external-dependencies ^:mutable refs]
  IDependencyNode
  (-get-value [_]
    value)                                ;unwrap references
  (set-value! [_ new-value]
    (set! value new-value))
  (-dependents [_]
    subscribers)
  (-data-selector [_]
    selector)
  (clear-node! [this graph]
    (set! value NODE-NOT-RESOLVED-SENTINEL)
    (when (satisfies? proto/StatefulSelector selector)
      (set! state (proto/create selector)))
    (set! stale? true)
    (set! subscribers #{}))
  impl/ReadPort
  (take! [this ^not-native handler]
    (if (not ^boolean (impl/active? handler))
      nil
      (if (not (nil? value))
        (let [_ (impl/commit handler)]
          (imp-chan/box value))
        (do
          ;(set! external-dependencies (conj external-dependencies handler))
          nil))))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#node {:value ")
    (pr-writer value writer opts)
    (-write writer "\n:state ")
    (pr-writer state writer opts)
    ;(-write writer " :dependents ")
    ;(pr-writer dependents #_(into #{} (map #(.-selector %)) dependents) writer opts)
    ;(-write writer " :refs ")
    ;(pr-writer refs #_(into #{} (map #(.-selector %)) refs) writer opts)
    (-write writer "}")))

(defn node
  ([sel] (node sel NODE-NOT-RESOLVED-SENTINEL))
  ([sel val]
   (->Node sel val nil true #{} nil #{})))


