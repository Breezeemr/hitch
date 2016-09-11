(ns hitch.dependent-transaction
  (:require [hitch.protocols :as proto]
            [cljs.core.async :as async]
            [clojure.set]
            [cljs.core.async.impl.protocols :as impl]))

(deftype TX [graph target ^:mutable requests]
  proto/IBatching
  (-request-invalidations [_ invalidations]
    (proto/-request-invalidations graph invalidations))
  (peek-invalidations [_]
    (proto/peek-invalidations graph))
  (take-invalidations! [_] (proto/take-invalidations! graph))
  proto/IDependencyGraph
  (peek-node [this data-selector]
    (proto/peek-node graph data-selector))
  (create-node! [this data-selector]
    (proto/create-node! graph data-selector))
  (subscribe-node [this data-selector]
    (set! requests (conj requests data-selector))
    (proto/get-or-create-node graph data-selector))
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
