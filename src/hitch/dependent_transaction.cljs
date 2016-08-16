(ns hitch.dependent-transaction
  (:require [hitch.protocols :as proto]
            [cljs.core.async :as async]
            [clojure.set]
            [cljs.core.async.impl.protocols :as impl]))

(deftype TX [graph target ^:mutable requests]
  proto/IBatching
  (-request-effects [_ effect]
    (proto/-request-effects graph effect))
  (-request-invalidations [_ invalidations]
    (proto/-request-invalidations graph invalidations))
  (take-effects! [_] (proto/take-effects! graph))
  (take-invalidations! [_] (proto/take-invalidations! graph))
  proto/IDependencyGraph
  (peek-node [this data-selector]
    (proto/peek-node graph data-selector))
  (create-node! [this data-selector]
    (proto/create-node! graph data-selector))
  (subscribe-node [this data-selector]
    (set! requests (conj requests data-selector))
    (let [n (proto/get-or-create-node graph data-selector)
          changes (proto/node-depend! n target)]
      (when-let [[new-effects new-invalidates] changes]
        (proto/-request-effects graph new-effects)
        (proto/-request-invalidations graph new-invalidates))
      n))
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

(defn run-tx-computation [graph selector node]
  (assert node)
  ;(prn "run-tx-computation" (.-state node))
  (let [dtransact (tx graph node)]
    (binding [proto/*read-mode* true]
      (let [computation (proto/-value selector dtransact (.-state node))
            new-value (if (satisfies? impl/ReadPort computation)
                        (async/poll! computation)
                        computation)]
        (doseq [retired-selector (retired-selectors (proto/get-tx node) dtransact)
                :let [retired-node (proto/peek-node graph retired-selector)]
                :when retired-node
                :let [changes (proto/node-undepend! retired-node node)]
                :when changes
                :let [[new-effects new-invalidates] changes]]
          (proto/-request-effects graph new-effects)
          (proto/-request-invalidations graph new-invalidates))
        (proto/set-tx! node dtransact)

        ;(prn "new val" selector new-value )
        new-value))
    ))