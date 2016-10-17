(ns hitch.graph
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.hook :refer [mkhook]]
            [hitch.mutable.graph :as mgraph]
            [hitch.mutable.node :refer [node]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async :as async]))


(def ^:dynamic *execution-mode* true)

;(declare apply-effects invalidate-nodes normalize-tx! schedule-actions)

(defn make-hook [graph selector]
  (let [h  (mkhook graph selector)]
    (oldproto/get-or-effect-graph graph selector)
    (oldproto/-add-external-dependent graph selector h)
    h
    ))


(defn dget!
  ([graph selector nf]
   (let [n (oldproto/get-or-effect-graph graph selector nf)]
     (oldproto/depend! graph selector)
     n)))

(defn hook
  ([graph selector-constructor] (make-hook graph (selector-constructor)))
  ([graph selector-constructor a] (make-hook graph (selector-constructor a)))
  ([graph selector-constructor a b] (make-hook graph (selector-constructor a b)))
  ([graph selector-constructor a b c] (make-hook graph (selector-constructor a b c)))
  ([graph selector-constructor a b c d] (make-hook graph (selector-constructor a b c d)))
  ([graph selector-constructor a b c d f] (make-hook graph (selector-constructor a b c d f)))
  ([graph selector-constructor a b c d f g] (make-hook graph (selector-constructor a b c d f g)))
  ([graph selector-constructor a b c d f g h] (make-hook graph (selector-constructor a b c d f g h))))

(defn hitch!
  ([graph nf selector-constructor]
   (if *execution-mode*
     (dget! graph (selector-constructor) nf)
     (oldproto/inline selector-constructor graph)))
  ([graph nf selector-constructor a]
   (if *execution-mode*
     (dget! graph (selector-constructor a) nf)
     (oldproto/inline selector-constructor graph a)))
  ([graph nf selector-constructor a b]
   (if *execution-mode*
     (dget! graph (selector-constructor a b) nf)
     (oldproto/inline selector-constructor graph a b)))
  ([graph nf selector-constructor a b c]
   (if *execution-mode*
     (dget! graph (selector-constructor a b c) nf)
     (oldproto/inline selector-constructor graph a b c)))
  ([graph nf selector-constructor a b c d]
   (if *execution-mode*
     (dget! graph (selector-constructor a b c d) nf)
     (oldproto/inline selector-constructor graph a b c d)))
  ([graph nf selector-constructor a b c d e]
   (if *execution-mode*
     (dget! graph (selector-constructor a b c d e) nf)
     (oldproto/inline selector-constructor graph a b c d e)))
  ([graph nf selector-constructor a b c d e f ]
   (if *execution-mode*
     (dget! graph (selector-constructor a b c d e f ) nf)
     (oldproto/inline selector-constructor graph a b c d e f )))
  ([graph nf selector-constructor a b c d e f g ]
   (if *execution-mode*
     (dget! graph (selector-constructor a b c d e f g) nf)
     (oldproto/inline selector-constructor graph a b c d e f g))))

(defn apply-effects [graph selector-effect-pairs]
  (oldproto/apply-commands graph selector-effect-pairs))