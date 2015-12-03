(ns hitch.dependent-transaction
  (:require [hitch.protocols   :as proto ]
            [clojure.set]))

(deftype TX [graph ^:mutable requests]
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
  proto/IEvaluateRequest
  (eval-request [this selector-constructor parent-node]
    (let [sel (new selector-constructor)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))
  (eval-request [this selector-constructor parent-node a]
    (let [sel (new selector-constructor a)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))
  (eval-request [this selector-constructor parent-node a b]
    (let [sel (new selector-constructor a b)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))
  (eval-request [this selector-constructor parent-node a b c]
    (let [sel (new selector-constructor a b c)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))
  (eval-request [this selector-constructor parent-node a b c d]
    (let [sel (new selector-constructor a b c d)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))
  (eval-request [this selector-constructor parent-node a b c d f]
    (let [sel (new selector-constructor a b c d f)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))
  (eval-request [this selector-constructor parent-node a b c d f g]
    (let [sel (new selector-constructor a b c d f g)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))
  (eval-request [this selector-constructor parent-node a b c d f g h]
    (let [sel (new selector-constructor a b c d f g h)
          _   (set! requests (conj requests sel))
          node (proto/get-or-create-node graph sel parent-node)]
      (proto/get-value node)))

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