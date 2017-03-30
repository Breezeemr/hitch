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

(defn mk-xhr [url method serializer deserializer content headers withcreds cb]
  (let [xhr (XhrIo.)]
    (when withcreds
      (.setWithCredentials xhr true))
    (events/listen xhr EventType/SUCCESS
      (if deserializer
        (fn [e] (cb [:ok (deserializer (.. e -target (getResponseText)))]))
        (fn [e] (cb [:ok (.. e -target (getResponseText))]))))
    (events/listen xhr EventType/ERROR
      (fn [e] (cb [:error (.. e -target (getLastError))])))
    (.send xhr (str url) method (if serializer
                                  (serializer content)
                                  content) (clj->js headers))
    #(.dispose xhr)))

(defn resolve-http-effect [{:keys [url method serializer deserializer content headers withcreds] :as http-selector}]
  (fn [gm]
    (let [aborter (mk-xhr url (str/upper-case (name method))
                          serializer deserializer content headers
                          withcreds
                          (fn [result]
                            (oldproto/apply-commands gm [[http-selector [::value result]]])))]
      (oldproto/apply-commands gm [[http-selector [::aborter aborter]]]))))

(defrecord HTTPSelector [url method serializer deserializer content headers withcreds]
  proto/StatefulSelector
  (create [s]
    (proto/->StateEffect {::state ::inflight} (resolve-http-effect s) nil))
  (destroy [s state]
    #_(when-some [abort (::aborter state)]
      (fn [_] (abort))))

  proto/CommandableSelector
  (command-accumulator
    [s state] [state nil])
  (command-step [s [state effect :as acc] command]
    (let [[kind x] command]
      (case kind
        ::refresh (if (= (::state state) ::inflight)
                    acc
                    [{::state ::inflight} (resolve-http-effect s)])
        ::aborter (if (= (::state state) ::inflight)
                    [(assoc state ::aborter x) effect]
                    ;; invalid!
                    acc)
        ::value (if (= (::state state) ::inflight)
                  [{::state ::finished
                    ::value x}
                   effect]
                  ;; invalid!
                  acc))))
  (command-result [s [state effect :as acc]]
    (proto/->StateEffect state effect nil))

  proto/Selector
  (value [this graph state]
    (if-some [result (::value state)]
      (proto/->SelectorValue result nil)
      (proto/map->SelectorUnresolved nil))))

(def http ->HTTPSelector)
