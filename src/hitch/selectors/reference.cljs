(ns hitch.selectors.reference
  (:require [hitch.protocols :as proto]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]))

(defrecord Reference [n]
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
    (graph/get-or-create-node! graph state)))

(def reference
  (reify proto/ISelectorFactory
    (-selector [_ n]
      (->Reference n))))


