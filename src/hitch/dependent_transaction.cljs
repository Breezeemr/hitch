(ns hitch.dependent-transaction
  (:require [hitch.protocols :as proto]
            [cljs.core.async :as async]
            [clojure.set]
            [cljs.core.async.impl.protocols :as impl]))

(deftype TX [graph ^:mutable requests]
  proto/IDependencyTracker
  (depend! [_ dependee dependent]
    (set! requests (conj requests dependent))
    (proto/depend! graph dependee dependent))
  (undepend! [_ dependee dependent]
    (proto/undepend! graph dependee dependent))
  proto/IDependencyGraph
  (get-node [this data-selector]
    (set! requests (conj requests data-selector))
    (proto/get-node graph data-selector))
  (add-node! [this data-selector new-node]
    (proto/add-node! graph data-selector new-node))
  #_(clear-graph! [this]
                  (proto/clear! graph))
  #_(gc [this data-selector]
        #_(do (doseq [d (proto/selector-dependencies data-selector)]
                (proto/undepend! this d))
              true))
  )


(defn tx [graph]
  (TX. graph #{}))

(defn new-selectors [oldtx newtx]
  (if oldtx
    (clojure.set/difference (.-requests newtx) (.-requests oldtx))
    newtx))

(defn retired-selectors [oldtx newtx]
  (if oldtx
    (clojure.set/difference (.-requests oldtx) (.-requests newtx))
    #{}))

(defn run-tx-computation [graph selector node]
  (let [dtransact (tx graph)]
    (binding [proto/*current-node* node]
      (when-let [computation (proto/-value selector dtransact (.-state node))]
        (let [new-value (if (satisfies? impl/ReadPort computation)
                          (async/poll! computation)
                          computation)]
          (doseq [retired-selector (retired-selectors (proto/get-tx node) dtransact)
                  :let [retired-node (proto/get-node graph retired-selector)]
                  :when retired-node]
            (proto/undepend! graph retired-node node))
          (proto/set-tx! node dtransact)
          new-value)))
    ))