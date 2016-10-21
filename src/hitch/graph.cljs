(ns hitch.graph
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]))


(def ^:dynamic *execution-mode* true)

;(declare apply-effects invalidate-nodes normalize-tx! schedule-actions)
(deftype Hook [graph selector cb]
  oldproto/ExternalDependent
  (-change-notify [this]
    (let [val (get graph selector oldproto/NOT-FOUND-SENTINEL)]
      ;(prn "notify" selector-changed val)
      (when-not (identical? oldproto/NOT-FOUND-SENTINEL val)
        (oldproto/update-parents  graph this nil #{selector})
        (cb val)))))

(defn mkhook [graph selector cb]
  (->Hook graph selector cb))


(defn make-hook [graph cb selector]
  (let [val (oldproto/get-or-effect-graph graph selector oldproto/NOT-FOUND-SENTINEL)]
    (if (identical? oldproto/NOT-FOUND-SENTINEL val)
      (let [h  (mkhook graph selector cb)]
        (oldproto/update-parents graph h #{selector} nil))
      (cb val)))
  nil)


(defn dget!
  ([graph selector nf]
   (let [n (oldproto/get-or-effect-graph graph selector nf)]
     (oldproto/depend! graph selector)
     n)))

(defn hook
  ([graph cb selector-constructor] (make-hook graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (make-hook graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (make-hook graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (make-hook graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (make-hook graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (make-hook graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (make-hook graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (make-hook graph cb (selector-constructor a b c d f g h))))

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

(def berror (js/Error. "bomb"))
(def bomb (reify
            IDeref
            (-deref [_]
              (throw berror))
            IPending
            (-realized? [x]
              false)))
(deftype box [v]
  IDeref
  (-deref [_]
    v)
  IPending
  (-realized? [x]
    true))

(defn select
  ([graph selector-constructor]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e f ]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e f g ]
   (let [v (hitch! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f g)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v)))))

(defn apply-effects [graph selector-effect-pairs]
  (oldproto/apply-commands graph selector-effect-pairs))