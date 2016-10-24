(ns hitch.selectors.http
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.mutable.node :as node]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]
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
      (fn [e] (cb (deserializer (some-> e .-target .getResponseText)))))
    (events/listen xhr EventType/ERROR
      (fn [e] e))
    (.send xhr (str url) (meths method) (if serializer
                                          (serializer content)
                                          content) headers)
    ))

(defrecord HTTPSelector [url method serializer deserializer content headers]
  proto/StatefulSelector
  (create [selector]
    {:val    oldproto/NOT-FOUND-SENTINEL
     :action true})
  (destroy [selector state])
  proto/InformedSelector
  proto/CommandableSelector
  (command-accumulator
    [s state] state)
  (command-step [s acc event]
    (let [[key] event]
      (case key
        :add-dep (update acc :deps conj (second event))
        :remove-dep acc
        :set-value (let [new-value (second event)]
                     (assoc acc :val new-value)))))
  (command-result [s acc]
    (if (:action acc)
      (proto/->StateEffect (dissoc acc :action)
        (fn [simple-graph effect-sink]
          (mk-xhr url method serializer deserializer content headers
            (fn [result]
              (effect-sink [[s [:set-value result]]]))))
        nil)
      (proto/->StateEffect acc nil nil)))
  proto/Selector
  (value [this graph state]
    ;(prn "state" state)
    (if state
      (if (identical? (:val state) oldproto/NOT-FOUND-SENTINEL)
        (proto/->SelectorUnresolved nil)
        (proto/->SelectorValue (:val state) nil))
      (proto/->SelectorUnresolved nil))))

(def http
  (reify
    IFn
    (-invoke [this url method serializer deserializer content headers]
      (->HTTPSelector url method serializer deserializer content headers))))
