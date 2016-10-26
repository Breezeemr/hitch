(ns hitch.selectors.kv-store
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.selector :refer-macros [defselector]]))

(defselector keystore-get [graph sel k]
  (get (graph/select-sel! graph sel) k))

(defrecord KVStoreServiceSelector [keyspace]
  proto/StatefulSelector
  (create [selector]
    (proto/->StateEffect oldproto/NOT-FOUND-SENTINEL nil nil))
  (destroy [selector state]
    nil)
  proto/CommandableSelector
  (command-accumulator
    [s state] state)
  (command-step [s acc command]
    (case (first command)
      :clear oldproto/NOT-FOUND-SENTINEL
      :set-value (second command)))
  (command-result [s acc]
    (proto/->StateEffect acc nil nil))
  proto/Selector
  (value [this graph state]
    (if (identical? state oldproto/NOT-FOUND-SENTINEL)
      (proto/->SelectorUnresolved nil)
      (proto/->SelectorValue state nil))))

(def keyspace ->KVStoreServiceSelector)

