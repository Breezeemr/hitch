(ns hitch.nodes.dynamic-node
  (:require [hitch.protocols   :as proto ]
            [hitch.dependent-transaction :as dtx]
            ))


(deftype DependencyNode [data-selector ^:mutable value ^:mutable dependents ^:mutable aliased-by  ^:mutable refs]
  proto/IDependencyNode
  (-get-value [_]
    (proto/get-value value))                                 ;unwrap references
  (set-value! [_ new-value]
    (set! value new-value))
  (make-value [current-node graph]
    (let [dtransact (dtx/tx graph)
          newvalue (proto/selector-invoke data-selector dtransact current-node)]
      ;(dtx/new-selectors refs dtransact)
      (doseq [retired-selector (dtx/retired-selectors refs dtransact)
              :let [node (proto/get-node graph retired-selector)]
              :when node]
        (proto/undepend! node current-node))
      (set! refs dtransact)
      newvalue))
  (-dependents [_]
    dependents)
  (-data-selector [_]
    data-selector)
  (-aliased-by [_]
    aliased-by)
  (clear-node! [this graph]
    (when (proto/dnode? (.-value this))
      (proto/release-dependency graph this (.-value this)))
    (set! value nil)
    (set! dependents nil)
    (set! aliased-by  nil)
    (set! refs nil))
  proto/IDependencyTracker
  (depend! [this dependent]                 ;returns new?
    (if (contains? dependents dependent)
      false
      (do (set! dependents (conj dependents dependent))
          true)))
  (undepend! [this dependent]                              ;returns last-removed?
    (let [newdeps (disj dependents dependent)]
      (set! dependents newdeps)
      (if (= #{} newdeps)
        true
        false)))
  (alias! [this alias-target]                 ;returns new?
    (if (contains? aliased-by alias-target)
      false
      (do (set! aliased-by (conj aliased-by alias-target))
          true)))
  (unalias! [this alias-target]                              ;returns last-removed?
    (let [newaliases (disj aliased-by alias-target)]
      (set! aliased-by newaliases)
      (if (empty? newaliases)
        true
        false)))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#node {:value ")
    (pr-writer value writer opts)
    ;(-write writer " :dependents ")
    ;(pr-writer dependents #_(into #{} (map #(.-selector %)) dependents) writer opts)
    ;(-write writer " :refs ")
    ;(pr-writer refs #_(into #{} (map #(.-selector %)) refs) writer opts)
    (-write writer "}")))



(defn dep-node [data-selector]
  (assert (satisfies? proto/ISelector data-selector))
  (DependencyNode. data-selector :hitch/not-loaded #{} #{} nil))

(def dummynode (DependencyNode. nil true #{} #{} nil))

(defrecord FN0 [f]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (f graph current-node)))
(defrecord FN1 [f a]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (f graph current-node a)))
(defrecord FN2 [f a b]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (f graph current-node a b)))
(defrecord FN3 [f a b c]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (f graph current-node a b c)))
(defrecord FN4 [f a b c d]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (f graph current-node a b c d)))
(defrecord FN5 [f a b c d e]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (f graph current-node a b c d e)))
(defrecord FN6 [fn a b c d e f]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (fn graph current-node a b c d e f)))
(defrecord FN7 [fn a b c d e f g]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (fn graph current-node a b c d e f g)))
(defrecord FNrest [f a b c d e rest]
  proto/ISelector
  (selector-init [this graph current-node])
  (selector-ready? [this graph current-node] true)
  (selector-invoke [this graph current-node]
    (apply f graph current-node a b c d (conj rest e))))


(defn fn0 [fn] (FN0. fn))
(defn fn1 [fn a] (FN1. fn a ))
(defn fn2 [fn a b](FN2. fn a b))
(defn fn3 [fn a b c](FN3. fn a b c))
(defn fn4 [fn a b c d](FN4. fn a b c d))
(defn fn5 [fn a b c d e](FN5. fn a b c d e))
(defn fn6 [fn a b c d e f](FN6. fn a b c d e f))
(defn fn7 [fn a b c d e f g] (FN7. fn a b c d e f g))
(defn fnrest [f a b c d e rest] (FNrest. f a b c d e rest))


(defrecord DynamicSelector [sfn]
  proto/IGraphlessEval
  (raw-eval [selector-constructor graph parent-node]
    (sfn graph parent-node))
  (raw-eval [selector-constructor graph parent-node a]
    (sfn graph parent-node a))
  (raw-eval [selector-constructor graph parent-node a b]
    (sfn graph parent-node a b))
  (raw-eval [selector-constructor graph parent-node a b c]
    (sfn graph parent-node a b c))
  (raw-eval [selector-constructor graph parent-node a b c d]
    (sfn graph parent-node a b c d))
  (raw-eval [selector-constructor graph parent-node a b c d f]
    (sfn graph parent-node a b c d f))
  (raw-eval [selector-constructor graph parent-node a b c d f g]
    (sfn graph parent-node a b c d f g))
  (raw-eval [selector-constructor graph parent-node a b c d f g h]
    (sfn graph parent-node a b c d f g h))
  proto/ISelectorConstructor
  (construct-selector [selector-constructor]
    (fn0 sfn))
  (construct-selector [selector-constructor a]
    (fn1 sfn a))
  (construct-selector [selector-constructor a b]
    (fn2 sfn a b))
  (construct-selector [selector-constructor a b c]
    (fn3 sfn a b c))
  (construct-selector [selector-constructor a b c d]
    (fn4 sfn a b c d))
  (construct-selector [selector-constructor a b c d f]
    (fn5 sfn a b c d f))
  (construct-selector [selector-constructor a b c d f g]
    (fn6 sfn a b c d f g))
  (construct-selector [selector-constructor a b c d f g h]
    (fn7 sfn a b c d f g h)))

 (defn make-dselector [f]
   (->DynamicSelector f))

