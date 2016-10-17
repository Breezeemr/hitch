(ns hitch.nodes.simple
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.dependent-transaction :as dtx]
            [cljs.core.async :as async]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as imp-chan]
            hitch.values))


(defonce NODE-NOT-RESOLVED-SENTINEL
         (reify Object
           (toString [this] "NODE-NOT-RESOLVED-SENTINEL")
           IPrintWithWriter
           (-pr-writer [_ writer opts]
             (-write writer "#NODE-NOT-RESOLVED-SENTINEL"))))

(deftype Node [selector ^:mutable value ^:mutable state ^:mutable stale? ^:mutable subscribers ^:mutable external-dependencies ^:mutable refs]
  oldproto/IDependencyNode
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


