(ns hitch.selectors.http
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [goog.events :as events]
            [goog.net.EventType :as EventType]
            [clojure.string :as str])
  (:import (goog.net XhrIo)))

(def ^:private meths
  {:get    "GET"
   :put    "PUT"
   :post   "POST"
   :delete "DELETE"})

(defn mk-xhr [url method serializer deserializer content headers cb]
  (let [xhr (XhrIo.)]
    ;(.setWithCredentials xhr true)
    (events/listen xhr EventType/SUCCESS
      (if deserializer
        (fn [e] (cb [:ok (deserializer (.. e -target (getResponseText)))]))
        (fn [e] (cb [:ok (.. e -target (getResponseText))]))))
    (events/listen xhr EventType/ERROR
      (fn [e] (cb [:error (.. e -target (getLastError))])))
    (.send xhr (str url) method (if serializer
                                  (serializer content)
                                  content) headers)
    #(.dispose xhr)))

(defrecord HTTPSelector [url method serializer deserializer content headers]
  proto/StatefulSelector
  (create [s]
    (let [effect (fn [gm]
                   (let [aborter (mk-xhr url (str/upper-case (name method))
                                   serializer deserializer content headers
                                   (fn [result]
                                     (oldproto/apply-commands gm [[s [::value result]]])))]
                     (oldproto/apply-commands gm [[s [::aborter aborter]]])))]
      (proto/->StateEffect {} effect nil)))
  (destroy [s state]
    (when-some [abort (::aborter state)]
      (fn [_] (abort))))

  proto/CommandableSelector
  (command-accumulator
    [s state] state)
  (command-step [s acc command]
    (let [[kind x] command]
      (case kind
        ::aborter (assoc acc ::aborter x)
        ::value (-> (assoc acc ::value x)
                    (dissoc ::aborter ::error)))))
  (command-result [s acc]
    (proto/->StateEffect acc nil nil))

  proto/Selector
  (value [this graph state]
    (if-some [result (::value state)]
      (proto/->SelectorValue result nil)
      (proto/map->SelectorUnresolved nil))))

(def http ->HTTPSelector)
