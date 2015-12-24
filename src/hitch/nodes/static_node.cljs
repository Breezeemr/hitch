(ns hitch.nodes.static-node
  (:require [hitch.protocols   :as proto ]
            [hitch.dynamic-dependency :as dyn :include-macros true]))

(deftype DependencyNode [data-selector ^:mutable value ^:mutable dependents ^:mutable aliased-by  ^:mutable refs]
  proto/IDependencyNode
  (-get-value [_]
    (proto/get-value value))                                 ;unwrap references
  (set-value! [_ new-value]
    (set! value new-value))
  (make-value [current-node graph]
    (if (proto/selector-ready? data-selector graph current-node)
      (proto/selector-invoke data-selector graph current-node)
      :hitch/not-loaded))
  (-dependents [_]
    dependents)
  (-data-selector [_]
    data-selector)
  (-aliased-by [_]
    aliased-by)
  (clear-node! [this graph]
    (when (proto/dnode? (.-value this))
      (proto/release-dependency graph this (.-value this) nil))
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

