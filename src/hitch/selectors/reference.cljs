(ns hitch.selectors.reference
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]))

(defrecord Reference [n]
  oldproto/StatefulSelector
  (init [selector] nil)
  (clear [selector state])
  oldproto/SelectorEffects
  (-apply [selector old-state effect]
    (let [[key newvalue] effect]
      (case key
        :set-value [(second effect)])))
  oldproto/SelectorValue
  (-value [selector graph state]
    (graph/get-or-create-node! graph state)))

(def reference
  (reify oldproto/ISelectorFactory
    (-selector [_ n]
      (->Reference n))))


