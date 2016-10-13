(ns hitch.selectors.http
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.values :refer [->Realized ->NotRealized]]
            [hitch.nodes.simple :as node]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]
            [cljs.core.async :as async]
            [goog.events :as events]
            [goog.net.EventType :as EventType])
  (:import (goog.net XhrIo)
           goog.async.nextTick))

(def ^:private meths
  {:get    "GET"
   :put    "PUT"
   :post   "POST"
   :delete "DELETE"})

(defn mk-xhr [url method serializer deserializer content headers cb]
  (let [xhr (XhrIo.)]
    ;(.setWithCredentials xhr true)
    (events/listen xhr EventType/SUCCESS
                   (fn [e] (cb (deserializer (some-> e  .-target .getResponseText)))))
    (events/listen xhr EventType/ERROR
                   (fn [e] e))
    (.send xhr (str url) (meths method) (if serializer
                                          (serializer content)
                                          content) headers)
    ))

(defrecord HTTPSelector [url method serializer deserializer content headers]
  oldproto/StatefulSelector
  (init [selector]
    {:val    oldproto/NIL-SENTINEL
     :action true})
  (clear [selector state])
  oldproto/InformedSelector
  oldproto/EffectableSelector
  (effect-accumulator
    [s state] state)
  (effect-step [s acc event]
    (let [[key] event]
      (case key
        :add-dep (update acc :deps conj (second event))
        :remove-dep acc
        :set-value (let [new-value (second event)]
                     (assoc acc :val new-value)))))
  (effect-result [s acc]
    (if (:action acc)
      (oldproto/->EffectResultAction (dissoc acc :action)
                                     (fn [simple-graph effect-sink]
                                    (mk-xhr url method serializer deserializer content headers
                                            (fn [result]
                                              (effect-sink [[s [:set-value result]]])))))
      (oldproto/->EffectResult acc)))
  oldproto/SelectorValue
  (-value [this graph state]
    ;(prn "state" state)
    (if (identical? (:val state) oldproto/NIL-SENTINEL)
      (->NotRealized nil)
      (->Realized (:val state) nil))))

(def http
  (reify
    IFn
    (-invoke [this graph url method serializer deserializer content headers]
      (assert nil "alias is stateful and should not be evaled"))
    oldproto/ISelectorFactory
    (-selector [this url method serializer deserializer content headers]
      (->HTTPSelector url method serializer deserializer content headers))))
