(ns hitch.nodes.simple
  (:require [hitch.protocols :as proto]
            [hitch.dependent-transaction :as dtx]
            [cljs.core.async :as async]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as imp-chan]))




(deftype Node [selector ^:mutable value ^:mutable state ^:mutable stale? ^:mutable subscribers ^:mutable one-time-subscribers ^:mutable refs]
  proto/IDependencyNode
  (-get-value [_]
    (proto/get-value value))                                ;unwrap references
  (set-value! [_ new-value]
    (set! value new-value))
  (-dependents [_]
    subscribers)
  (-take-one-time-dependents! [_]
    ;(prn "one" one-time-subscribers)
    (let [ret one-time-subscribers]

      (set! one-time-subscribers #{})
      ;(prn "ret" ret)
      ret))
  (-data-selector [_]
    selector)
  (clear-node! [this graph]
    (set! value nil)
    (set! stale? true)
    (set! subscribers #{}))
  proto/IDynamicDepNode
  (get-tx [self] refs)
  (set-tx! [self newtx] (set! refs newtx))
  impl/ReadPort
  (take! [this ^not-native handler]
    (if (not ^boolean (impl/active? handler))
      nil
      (if (not (nil? value))
        (let [_ (impl/commit handler)]
          (imp-chan/box value))
        (do
          (set! one-time-subscribers (conj one-time-subscribers handler))
          nil))))
  proto/ISubscriber
  (-recalculate! [node graph]
    (let [new-value (dtx/run-tx-computation graph selector node)
          ret (cond
                (and value (nil? new-value)) :stale
                (not= value new-value) (do #_(prn "changein") (set! value new-value) :value-changed)
                :default :value-unchanged)]

      ret))
  proto/INodeDependencyTracker
  (node-depend! [this dependent]                            ;returns new?
    (if (contains? subscribers dependent)
      nil
      (do (set! subscribers (conj subscribers dependent))
          (let [dep-sel (proto/-data-selector dependent)]
            (when (satisfies? proto/InformedSelector selector)
              (let [[new-state new-effects] (proto/-apply selector state (proto/dependency-added selector dep-sel))]
                (if (not= new-state state)
                  (do
                    (set! state new-state)
                    [new-effects #{this}])
                  [new-effects])
                ))))))
  (node-undepend! [this dependent]                          ;returns last-removed?
    (let [newdeps (disj subscribers dependent)]
      (when (not= subscribers newdeps)
        (do
          (set! subscribers newdeps)
          (let [dep-sel (proto/-data-selector dependent)]
            (when (satisfies? proto/InformedSelector selector)
              (let [[new-state new-effects] (proto/-apply selector state (proto/dependency-removed selector dep-sel))]
                (if (not= new-state state)
                  (do
                    (set! state new-state)
                    [new-effects #{this}])
                  [new-effects]))))))))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#node {:value ")
    (pr-writer value writer opts)
    ;(-write writer " :dependents ")
    ;(pr-writer dependents #_(into #{} (map #(.-selector %)) dependents) writer opts)
    ;(-write writer " :refs ")
    ;(pr-writer refs #_(into #{} (map #(.-selector %)) refs) writer opts)
    (-write writer "}")))

(defn node
  ([sel] (node sel nil))
  ([sel val]
   (->Node sel val nil true #{} #{} #{})))


