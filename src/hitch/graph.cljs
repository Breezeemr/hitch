(ns hitch.graph
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graphs.mutable :as mgraph]
            [hitch.nodes.simple :refer [node]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async :as async]))


(def ^:dynamic *execution-mode* true)

;(declare apply-effects invalidate-nodes normalize-tx! schedule-actions)

(defn make-hook [graph selector]
  (let [n (oldproto/get-or-create-node graph selector)
        h  (oldproto/mkhook n)]
    (oldproto/-add-external-dependent n h)
    h
    #_(if-some [val (.-value n)]
        (async/put! prom val)
        (let [h (oldproto/->Hook n nil)]
          (prn "made hook")
          (oldproto/-add-external-dependent n h)))
    ;prom
    ))


(defn dget!
  ([graph selector]
   (dget! graph selector nil))
  ([graph selector nf]
   (let [n (oldproto/get-or-create-node graph selector nf)]
     (oldproto/depend! graph selector)
     n)))

(defn try-to-get-node [dependency-graph data-selector]
  (get dependency-graph data-selector))

(defn hitch-node
  ([graph selector-constructor] (dget! graph (selector-constructor)))
  ([graph selector-constructor a] (dget! graph (selector-constructor a)))
  ([graph selector-constructor a b] (dget! graph (selector-constructor a b)))
  ([graph selector-constructor a b c] (dget! graph (selector-constructor a b c)))
  ([graph selector-constructor a b c d] (dget! graph (selector-constructor a b c d)))
  ([graph selector-constructor a b c d f] (dget! graph (selector-constructor a b c d f)))
  ([graph selector-constructor a b c d f g] (dget! graph (selector-constructor a b c d f g)))
  ([graph selector-constructor a b c d f g h] (dget! graph (selector-constructor a b c d f g h))))

(defn hook-node
  ([graph selector-constructor] (make-hook graph (selector-constructor)))
  ([graph selector-constructor a] (make-hook graph (selector-constructor a)))
  ([graph selector-constructor a b] (make-hook graph (selector-constructor a b)))
  ([graph selector-constructor a b c] (make-hook graph (selector-constructor a b c)))
  ([graph selector-constructor a b c d] (make-hook graph (selector-constructor a b c d)))
  ([graph selector-constructor a b c d f] (make-hook graph (selector-constructor a b c d f)))
  ([graph selector-constructor a b c d f g] (make-hook graph (selector-constructor a b c d f g)))
  ([graph selector-constructor a b c d f g h] (make-hook graph (selector-constructor a b c d f g h))))

(defn hitch-eval
  ([graph selector-constructor] (oldproto/inline selector-constructor graph))
  ([graph selector-constructor a] (oldproto/inline selector-constructor graph a))
  ([graph selector-constructor a b] (oldproto/inline selector-constructor graph a b))
  ([graph selector-constructor a b c] (oldproto/inline selector-constructor graph a b c))
  ([graph selector-constructor a b c d] (oldproto/inline selector-constructor graph a b c d))
  ([graph selector-constructor a b c d f] (oldproto/inline selector-constructor graph a b c d f))
  ([graph selector-constructor a b c d f g] (oldproto/inline selector-constructor graph a b c d f g))
  ([graph selector-constructor a b c d f g h] (oldproto/inline selector-constructor graph a b c d f g h)))



(defn hitch
  ([graph selector-constructor]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor))
  ([graph selector-constructor a]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a))
  ([graph selector-constructor a b]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b))
  ([graph selector-constructor a b c]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c))
  ([graph selector-constructor a b c d]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d))
  ([graph selector-constructor a b c d f]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f))
  ([graph selector-constructor a b c d f g]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g))
  ([graph selector-constructor a b c d f g h]
   ((if *execution-mode* hitch-node hitch-eval) graph selector-constructor a b c d f g h)))

(defn hook
  ([graph selector-constructor] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor))
  ([graph selector-constructor a] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a))
  ([graph selector-constructor a b] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b))
  ([graph selector-constructor a b c] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c))
  ([graph selector-constructor a b c d] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d))
  ([graph selector-constructor a b c d f] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d f))
  ([graph selector-constructor a b c d f g] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d f g))
  ([graph selector-constructor a b c d f g h] ((if *execution-mode* hook-node hitch-eval) graph selector-constructor a b c d f g h)))

(defn apply-effects [graph selector-effect-pairs]
  (oldproto/apply-commands graph selector-effect-pairs))