(ns hitch.selectors.mutable-var
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]))

(defrecord MutableVar [n]
  proto/StatefulSelector
  (create [selector] nil)
  (destroy [selector state])
  proto/CommandableSelector
  (command-accumulator
    [s state] state)
  (command-step [s acc event]
    (let [[key] event]
      (case key
        :set-value (second event)
        :clear oldproto/NOT-FOUND-SENTINEL)))
  (command-result [s acc]
    (proto/->StateEffect acc nil nil))
  proto/Selector
  (value [selector graph state]
    (if (identical? oldproto/NOT-FOUND-SENTINEL state)
      (proto/->SelectorUnresolved nil)
      (proto/->SelectorValue state nil))))

(def mutable-var
  (reify IFn
    (-invoke [_ n]
      (->MutableVar n))))
