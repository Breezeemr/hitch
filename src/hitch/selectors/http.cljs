(ns hitch.selectors.http
  (:require [hitch.protocols :as proto]
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
    (.setWithCredentials xhr true)
    (events/listen xhr EventType/SUCCESS
                   (fn [e] (cb (deserializer e))))
    (events/listen xhr EventType/ERROR
                   (fn [e] e))
    (.send xhr (str url) (meths method) (serializer content) headers)
    ))

(defrecord HTTPSelector [url method serializer deserializer content headers]
  proto/StatefulSelector
  (init [selector]
    {:val proto/NIL-SENTINAL})
  (clear [selector state])
  proto/InformedSelector
  proto/EffectableSelector
  (effect-accumulator
    [s state] state)
  (effect-step [s acc event]
    ;(prn "effect " event)
    (let [[key] event]
      (case key
        :add-dep (if (empty? (:deps acc))
                   (-> acc
                       (update :deps conj (second event))
                       (assoc :action true))
                   (update acc :deps conj (second event)))
        :remove-dep acc
        :set-value (let [new-value (second event)]
                     (assoc acc :val new-value)))))
  (effect-result [s acc]
    (if (:action acc)
      (proto/->EffectResultAction (dissoc acc :action)
                                  (fn [simple-graph effect-sink]
                                    (mk-xhr url method serializer deserializer content headers
                                            (fn [result]
                                              (effect-sink [[s [:set-value result]]])))))
      (proto/->EffectResult acc)))
  proto/SelectorValue
  (-value [this graph state]
    ;(prn "state" state)
    (if (identical? (:val state) proto/NIL-SENTINAL)
      (->NotRealized nil)
      (->Realized (:val state) nil))))
