(ns hitch.selectors.kv-store
  (:refer-clojure :exclude [key])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.values :refer [->Realized ->NotRealized]]
            [hitch.nodes.node :as node :refer [NODE-NOT-RESOLVED-SENTINEL]]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]
            [cljs.core.async :as async])
  )

(declare  keyspace)
;(defrecord KeySelector [ks k]
;  proto/Selector
;  (value [this graph state]
;    (let [keyspace-selector (proto/-selector keyspace ks)]
;      (if-let [kstore (some-> (proto/peek-node graph keyspace-selector)
;                              async/poll!)]
;
;        (->Realized (get kstore k) #{keyspace-selector})
;        (->NotRealized #{keyspace-selector})))
;    ))
;
;(def key
;  (reify
;    IFn
;    (-invoke [this graph ks k]
;      (assert nil "alias is stateful and should not be evaled"))
;    proto/ISelectorFactory
;    (-selector [this ks k]
;      (->KeySelector ks k))))

(defselector key [graph sel k]
  (get (graph/dget! graph sel {}) k)
  )

(defrecord KVStoreServiceSelector [keyspace]
  proto/StatefulSelector
  (create [selector]
    {:val  oldproto/NIL-SENTINEL
     :deps #{}})
  (destroy [selector state])
  proto/InformedSelector
  proto/CommandableSelector
  (command-accumulator
    [s state] state)
  (command-step [s acc event]
    ;(prn "effect " event)
    (let [[key] event]
      (case key
        :clear (assoc acc :val NODE-NOT-RESOLVED-SENTINEL)
        :add-dep (update acc :deps conj (second event))
        :remove-dep (update acc :deps disj (second event))
        :set-value (let [new-value (second event)]
                     (assoc acc :val new-value)
                     ))))
  (command-result [s acc]
    ;(prn "acc" acc)
    (proto/->State acc))
  proto/Selector
  (value [this graph state]
    ;(prn "state" state)
    (if (identical? (:val state) oldproto/NIL-SENTINEL)
      (->NotRealized nil)
      (->Realized (:val state) nil))))

(def keyspace
  (reify
    IFn
    (-invoke [this ks]
      (->KVStoreServiceSelector ks))))

