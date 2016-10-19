(ns hitch.hook
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as imp-chan]))

(deftype Hook [graph selector ^:mutable handlers]
  oldproto/ExternalDependent
  (-change-notify [this _ selector-changed]

    (let [val (get graph selector oldproto/NOT-FOUND-SENTINEL)]
      ;(prn "notify" selector-changed val)
      (when-not (identical? oldproto/NOT-FOUND-SENTINEL val)
        (oldproto/update-parents  graph this nil #{selector})
        (run! (fn [handler]
                (let [real-handler (impl/commit handler)]
                  (real-handler val)))
              handlers))))
  impl/ReadPort
  (take! [this ^not-native new-handler]
    (if (not ^boolean (impl/active? new-handler))
      nil
      (let [val (oldproto/get-or-effect-graph graph selector oldproto/NOT-FOUND-SENTINEL)]
        ;(prn "take"  val)
        (if (not (identical? val oldproto/NOT-FOUND-SENTINEL))
          (let [_ (impl/commit new-handler)]
            (imp-chan/box val))
          (do
            (set! handlers (conj handlers new-handler))
            nil))))))
(defn mkhook [graph selector]
  (->Hook graph selector #{}))
