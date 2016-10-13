(ns hitch.dependent-transaction
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [cljs.core.async :as async]
            [clojure.set]
            [cljs.core.async.impl.protocols :as impl]))

(deftype TX [graph target ^:mutable requests]
  oldproto/IBatching
  (-request-invalidations [_ invalidations]
    (oldproto/-request-invalidations graph invalidations))
  (peek-invalidations [_]
    (oldproto/peek-invalidations graph))
  (take-invalidations! [_] (oldproto/take-invalidations! graph))
  oldproto/IDependencyGraph
  (peek-node [this data-selector]
    (oldproto/peek-node graph data-selector))
  (create-node! [this data-selector]
    (oldproto/create-node! graph data-selector))
  (subscribe-node [this data-selector]
    (set! requests (conj requests data-selector))
    (oldproto/get-or-create-node graph data-selector))
  #_(clear-graph! [this]
                  (proto/clear! graph))
  #_(gc [this data-selector]
        #_(do (doseq [d (proto/selector-dependencies data-selector)]
                (proto/undepend! this d))
              true))
  )


(defn tx[graph target]
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
