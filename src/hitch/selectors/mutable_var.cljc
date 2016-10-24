(ns hitch.selectors.mutable-var
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]))

(defrecord MutableVar [name]
  proto/StatefulSelector
  (create [selector]
    (proto/->StateEffect oldproto/NOT-FOUND-SENTINEL nil nil))
  (destroy [selector state]
    nil)

  proto/CommandableSelector
  (command-accumulator [_ state]
    state)
  (command-step [_ _ event]
    (case (first event)
      :set-value (second event)
      :clear oldproto/NOT-FOUND-SENTINEL))
  (command-result [_ state]
    (proto/->StateEffect state nil nil))

  proto/Selector
  (value [_ _ state]
    (if (identical? state oldproto/NOT-FOUND-SENTINEL)
      (proto/->SelectorUnresolved nil)
      (proto/->SelectorValue state nil))))

(def mutable-var ->MutableVar)
