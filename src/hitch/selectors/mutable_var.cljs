(ns hitch.selectors.mutable-var
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.values :refer [->Realized ->NotRealized]]
            [hitch.selector :refer-macros [defselector]]))

(defrecord MutableVar [n]
  oldproto/StatefulSelector
  (init [selector] nil)
  (clear [selector state])
  oldproto/EffectableSelector
  (effect-accumulator
    [s state] state)
  (effect-step [s acc event]
    (let [[key] event]
      (case key
        :set-value (second event))))
  (effect-result [s acc]
    (oldproto/->EffectResult acc))
  oldproto/SelectorValue
  (-value [selector graph state]
    (if state
      (->Realized state nil)
      (->NotRealized nil))))

(def mutable-var
  (reify oldproto/ISelectorFactory
    (-selector [_ n]
      (->MutableVar n))))
