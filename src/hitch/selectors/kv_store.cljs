(ns hitch.selectors.kv-store
  (:refer-clojure :exclude [key])
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]

            [hitch.nodes.simple :as node]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]])
  )

(declare  keyspace)
(defrecord KeySelector [ks k]
  proto/SelectorEffects
  (-apply [selector old-state effect]
    (let [[key newvalue] effect]
      (case key
        :set-value [(second effect)])))
  proto/SelectorValue
  (-value [this graph state]
    (graph/hitch graph keyspace ks)
    state))

(def key
  (reify
    IFn
    (-invoke [this graph ks k]
      (assert nil "alias is stateful and should not be evaled"))
    proto/ISelectorFactory
    (-selector [this ks k]
      (->KeySelector ks k))))

(defrecord KVStoreServiceSelector [keyspace]
  proto/StatefulSelector
  (init [selector]
    {:val nil
     :deps #{}})
  (clear [selector state])
  proto/InformedSelector
  proto/SelectorEffects
  (-apply [selector old-state effect]
    (let [[key] effect]
      (case key
        :add-dep  [(update old-state :deps conj (second effect))]
        :remove-dep  [(update old-state :deps disj (second effect))]
        :set-value (let [new-value (second effect)]
                     [(assoc old-state :val new-value)
                      (into [] (comp
                                 (filter #(instance? KeySelector %))
                                 (map (fn [selector]
                                        [selector [:set-value (get new-value (:k selector))]])))
                            (:deps old-state))]))))
  proto/SelectorValue
  (-value [this graph state]
    keyspace))

(def keyspace
  (reify
    IFn
    (-invoke [this graph ks]
      (assert nil "alias is stateful and should not be evaled"))
    proto/ISelectorFactory
    (-selector [this ks]
      (->KVStoreServiceSelector ks))))

