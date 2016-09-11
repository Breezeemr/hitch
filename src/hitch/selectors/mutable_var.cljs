(ns hitch.selectors.mutable-var
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]
            [hitch.values :refer [->Realized ->NotRealized]]
            [hitch.selector :refer-macros [defselector]]))

(defrecord MutableVar [n]
  proto/StatefulSelector
  (init [selector] nil)
  (clear [selector state])
  proto/EffectableSelector
  (effect-accumulator
    [s state] state)
  (effect-step [s acc event]
    (let [[key] event]
      (case key
        :set-value (second event))))
  (effect-result [s acc]
    (proto/->EffectResult acc))
  proto/SelectorValue
  (-value [selector graph state]
    (if state
      (->Realized state nil)
      (->NotRealized nil))))

(def mutable-var
  (reify proto/ISelectorFactory
    (-selector [_ n]
      (->MutableVar n))))
