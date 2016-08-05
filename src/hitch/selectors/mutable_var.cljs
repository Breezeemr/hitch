(ns hitch.selectors.mutable-var
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]))

(defrecord MutableVar [n]
  proto/StatefulSelector
  (init [selector] nil)
  (clear [selector state])
  proto/SelectorEffects
  (-apply [selector old-state effect]
    (let [[key newvalue] effect]
      (case key
        :set-value [(second effect)])))
  proto/SelectorValue
  (-value [selector graph state]
    state))

(def mutable-var
  (reify proto/ISelectorFactory
    (-selector [_ n]
      (->MutableVar n))))
