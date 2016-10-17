(ns hitch.mutable.tx
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [cljs.core.async :as async]
            [clojure.set]
            [cljs.core.async.impl.protocols :as impl]))

(deftype TX [graph target ^:mutable requests]
  ILookup
  (-lookup [o data-selector]
    (-lookup o data-selector nil))
  (-lookup [o data-selector not-found]
    (-lookup graph data-selector not-found))
  oldproto/IBatching
  (-request-invalidations [_ invalidations]
    (oldproto/-request-invalidations graph invalidations))
  (peek-invalidations [_]
    (oldproto/peek-invalidations graph))
  (take-invalidations! [_] (oldproto/take-invalidations! graph))
  oldproto/IDependencyGraph
  (create-node! [this data-selector nf]
    (oldproto/create-node! graph data-selector nf))
  (apply-commands [_ selector-command-pairs]
    (oldproto/apply-commands graph selector-command-pairs))
  (depend! [this data-selector]
    (set! requests (conj requests data-selector)))
  (undepend! [this data-selector]
    (set! requests (disj requests data-selector)))
  (-add-external-dependent [this parent child]
    (oldproto/-add-external-dependent graph parent child))
  (-remove-external-dependent [this parent child]
    (oldproto/-remove-external-dependent graph parent child))
  (-get-external-dependents [this parent]
    (oldproto/-get-external-dependents graph parent))
  #_(clear-graph! [this]
                  (proto/clear! graph))
  #_(gc [this data-selector]
        #_(do (doseq [d (proto/selector-dependencies data-selector)]
                (proto/undepend! this d))
              true))
  )


(defn tx [graph target]
  (if (instance? TX graph)
    (TX. (.-graph graph) target #{})
    (TX. graph target #{})))

(defn new-selectors [oldtx newtx]
  (if oldtx
    (clojure.set/difference (.-requests newtx) (.-requests oldtx))
    newtx))

(defn retired-selectors [oldtx newtx]
  (if oldtx
    (clojure.set/difference (.-requests oldtx) (.-requests newtx))
    #{}))
