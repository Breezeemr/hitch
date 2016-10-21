(ns hitch.selector-tx-manager
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [clojure.set]))

(deftype EagerTX [graph selector ^:mutable requests]
  ILookup
  (-lookup [o data-selector]
    (-lookup o data-selector nil))
  (-lookup [o data-selector not-found]
    (-lookup graph data-selector not-found))
  oldproto/IDependTrack
  (dget-sel! [this data-selector nf]
    (set! requests (conj requests data-selector))
    (let [v (get this data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
      (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
        (if (satisfies? oldproto/IEagerSelectorResolve graph)
          (oldproto/attempt-eager-selector-resolution! graph data-selector nf)
          nf)
        v) ))
  (get-depends [this] requests)
  oldproto/IDependencyGraph
  (apply-commands [_ selector-command-pairs]
    (oldproto/apply-commands graph selector-command-pairs)))


(defn tx [graph target]
  (if (instance? EagerTX graph)
    (EagerTX. (.-graph graph) target #{})
    (EagerTX. graph target #{})))

(defn new-selectors [oldtx newtx]
  (if oldtx
    (clojure.set/difference (.-requests newtx) (.-requests oldtx))
    newtx))

(defn retired-selectors [oldtx newtx]
  (if oldtx
    (clojure.set/difference (.-requests oldtx) (.-requests newtx))
    #{}))
