(ns hitch.selectors.kv-store
  (:refer-clojure :exclude [key])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.values :refer [->Realized ->NotRealized]]
            [hitch.nodes.simple :as node]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]
            [cljs.core.async :as async])
  )

(declare  keyspace)
;(defrecord KeySelector [ks k]
;  proto/SelectorValue
;  (-value [this graph state]
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
  (get (-> (graph/hitch-sel graph sel)
           async/<!) k)
  )

(defrecord KVStoreServiceSelector [keyspace]
  oldproto/StatefulSelector
  (init [selector]
    {:val  oldproto/NIL-SENTINEL
     :deps #{}})
  (clear [selector state])
  oldproto/InformedSelector
  oldproto/EffectableSelector
  (effect-accumulator
    [s state] state)
  (effect-step [s acc event]
    ;(prn "effect " event)
    (let [[key] event]
      (case key
        :add-dep (update acc :deps conj (second event))
        :remove-dep (update acc :deps disj (second event))
        :set-value (let [new-value (second event)]
                     (assoc acc :val new-value)
                     ))))
  (effect-result [s acc]
    ;(prn "acc" acc)
    (oldproto/->EffectResult acc))
  oldproto/SelectorValue
  (-value [this graph state]
    ;(prn "state" state)
    (if (identical? (:val state) oldproto/NIL-SENTINEL)
      (->NotRealized nil)
      (->Realized (:val state) nil))))

(def keyspace
  (reify
    IFn
    (-invoke [this graph ks]
      (assert nil "alias is stateful and should not be evaled"))
    oldproto/ISelectorFactory
    (-selector [this ks]
      (->KVStoreServiceSelector ks))))

