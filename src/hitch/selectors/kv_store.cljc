(ns hitch.selectors.kv-store
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.tracking.halt :as halt]
            [hitch.selector #?(:cljs :refer-macros :default :refer) [defselector]]))

(defonce ^:private NOT-FOUND #?(:cljs (js/Object.) :default (Object.)))

(defselector keystore-get [graph sel k]
  (let [found (get @(graph/select-sel! graph sel) k NOT-FOUND)]
    (if (identical? found NOT-FOUND)
      (halt/halt!)
      found)))

(defrecord KVStoreServiceSelector [keyspace]
  proto/StatefulSelector
  (create [selector]
    (proto/->StateEffect {:deps {} } nil nil))
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

