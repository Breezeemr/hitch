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


(defn hook-sel [graph cb data-selector]
  (let [val (let [v (get graph data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
                 (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
                   (if (satisfies? oldproto/IEagerSelectorResolve graph)
                     (oldproto/attempt-eager-selector-resolution! graph data-selector oldproto/NOT-IN-GRAPH-SENTINEL)
                     oldproto/NOT-IN-GRAPH-SENTINEL)
                   v))]
    (if (identical? val oldproto/NOT-IN-GRAPH-SENTINEL)
      (let [h  (mkhook graph data-selector cb)]
        (oldproto/update-parents graph h #{data-selector} nil))
      (cb val)))
  nil)

(def dget-sel! oldproto/dget-sel!)

(defn hook
  ([graph cb selector-constructor] (hook-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-sel graph cb (selector-constructor a b c d f g h))))

(defn dget!
  ([graph nf selector-constructor]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor) nf)
     (oldproto/inline selector-constructor graph)))
  ([graph nf selector-constructor a]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a) nf)
     (oldproto/inline selector-constructor graph a)))
  ([graph nf selector-constructor a b]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b) nf)
     (oldproto/inline selector-constructor graph a b)))
  ([graph nf selector-constructor a b c]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c) nf)
     (oldproto/inline selector-constructor graph a b c)))
  ([graph nf selector-constructor a b c d]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d) nf)
     (oldproto/inline selector-constructor graph a b c d)))
  ([graph nf selector-constructor a b c d e]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d e) nf)
     (oldproto/inline selector-constructor graph a b c d e)))
  ([graph nf selector-constructor a b c d e f ]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d e f) nf)
     (oldproto/inline selector-constructor graph a b c d e f )))
  ([graph nf selector-constructor a b c d e f g ]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d e f g) nf)
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

(defn select-sel!
  ([graph selector]
   (let [v (dget-sel! graph selector oldproto/NOT-IN-GRAPH-SENTINEL)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v)))))

(defn select!
  ([graph selector-constructor]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e f ]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e f g ]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f g)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v)))))

(defn apply-commands [graph selector-command-pairs]
  (oldproto/apply-commands graph selector-command-pairs))