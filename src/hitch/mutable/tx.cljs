(ns hitch.mutable.tx
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
  (depend! [this data-selector]
    (set! requests (conj requests data-selector)))
  (get-depends [this] requests)
  oldproto/IDependencyGraph
  (create-node! [this data-selector nf]
    (oldproto/create-node! graph data-selector nf))
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
