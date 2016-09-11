(ns hitch.protocols
  (:require [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as imp-chan]))

(def ^:dynamic *read-mode* false)

(defonce NIL-SENTINAL (reify Object
                      (toString [this] "NIL-SENTINAL")))
(defn fixnil [v]
  (if (identical? v NIL-SENTINAL)
    nil
    v))
#_(defprotocol ISubscriber
  (-recalculate! [sub graph]))

(defprotocol ISelectorFactory
  (-selector [selector-factory] [selector-factory a] [selector-factory a b] [selector-factory a b c] [selector-factory a b c d] [selector-factory a b c d e] [selector-factory a b c d e f] [selector-factory a b c d e f g] [selector-factory a b c d e f g h]))

(defprotocol IDynamicDepNode
  (get-tx [this])
  (set-tx! [this tx]))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (peek-node [this data-selector] "gets node for dataselector")
  (subscribe-node [this data-selector] "gets node for dataselector")
  #_(create-node! [this data-selector]
                  "create node and follow init lifecycle")
  (create-node! [this data-selector]
                "adds node")
  (clear-graph! [this])
  (gc [this data-selector]
      "Schedule clean up of the resources for dataselector"))

#_(defprotocol INodeDependencyTracker
  "Implemented by function and component caches"
  (node-depend! [dependee dependent]
                "Dependency sources call this method if a tracker is bound in the current
                 context with dependencies that are encountered during query processing.")
  (node-undepend! [dependee dependent]))


(defprotocol InformedSelector
  "A marker protocol. When present, a :hitch.protocol/child-changes effect are
  added to the selector's effect queue which inform when child selectors begin
  to or cease depending on the current selector.")

(defprotocol ExternalDependent
  (-change-notify [this graph selector-changed]))

(defprotocol IDependencyNode
  "A utility API for tracking dependencies, allows us to provide more
   advanced options for assembling tracker policies"
  (-get-value [this]
              "Returns cached value if exists for params")
  (set-value! [this new-value]
              "Informs store that a particular params yeilds value given current store + deps")
  (-dependents [this]
               "The current dependencies encountered by this tracker")
  (-add-external-dependent [this dependent])
  (-remove-external-dependent [this dependent])
  (-get-external-dependents [this]
                            "The current dependencies encountered by this tracker")
  (-data-selector [this]
                  "The nodes that return this nodes value")
  (clear-node! [this graph]))

(defn get-value [x]
  (if (satisfies? IDependencyNode x)
    (-get-value x)
    x))
(defn get-dependents [node]
  (-dependents node))
(defn get-data-selector [node]
  (-data-selector node))


(defprotocol StatefulSelector
  (init [selector] ;=> state
        "Return opaque state object which should be associated with the selector.)

        This is an opportunity to initialize and acquire stateful or mutable
        resources.")

  (clear [selector state] ;=> nil
         "Clear any stateful or mutable resources. Return value is ignored.
         This is an opportunity to clean up before destruction."))

(defprotocol SelectorEffects
  (-apply [selector old-state effects]))

(defprotocol SilentSelector
  "If this protocol is present, child selectors values are *not* recalculated
  when this selector changes. Instead, this selector must mark them stale in
  the return value of effect-result")

(defrecord EffectResult [state])
(defrecord EffectResultAction [state action])

(defrecord EffectResultRefresh [state recalc-child-selectors])

(defrecord EffectError [accumulator pending-effects bad-effect error])

(defprotocol EffectableSelector
  "Allows a selector to receive effects and alter its state based on effects.
  These methods are only called if a selector has effects in the current
  transaction."
  (effect-accumulator
    [s state] "Return an opaque accumulator for effect-step. `state` will be
    nil if it has never been initialized via StatefulSelector or a previous
    effect-result call.")
  (effect-step [s accumulator event]
               "Accept an event and return a new accumulator. This method is called once
               per event.
               At any time this function may return an EffectError which the caller may
               use to determine how to handle the error. It should be possible in principle
               (although not necessarily in practice) to resume the effect reduction
               process using the data in an EffectError.")
  (effect-result [s accumulator]
                 "Return an StateAction after all effects have been processed, which
                 contains the new state and an action.
                 If this selector also implements SilentSelector, it may return an
                 StateActionUpdate instead which also returns a list of children to
                 invalidate."))



(defprotocol SelectorValue
  (-value [selector graph state]))


(defprotocol IBatching
  (-request-invalidations [graph invalidations])
  (peek-invalidations [graph])
  (take-invalidations! [graph]))


(defn get-or-create-node [graph data-selector]
  (if-let [n (peek-node graph data-selector)]
    n
    (let [n (create-node! graph data-selector)]
      (-request-invalidations graph data-selector)
      n)))

(defn get-temp-state [graph selector]
  (assert (satisfies? EffectableSelector selector) )
  (if-let [ts (get (.-tempstate graph) selector)]
    ts
    (let [node (get-or-create-node graph selector)
          ts (atom (effect-accumulator selector (.-state node)))]
      (set! (.-tempstate graph) (assoc (.-tempstate graph) selector ts))
      ts)))



(deftype Hook [n ^:mutable handlers]
  ExternalDependent
  (-change-notify [this graph selector-changed]
    (let [val (.-value n)]
      (-remove-external-dependent n this)
      (doseq [handler handlers]
        (let [real-handler (impl/commit handler)]
          (real-handler val))))
    )
  impl/ReadPort
  (take! [this ^not-native new-handler]
    (if (not ^boolean (impl/active? new-handler))
      nil
      (if (not (nil? (.-value n)))
        (let [_ (impl/commit new-handler)]
          (imp-chan/box (.-value n)))
        (do
          (set! handlers (conj handlers new-handler))
          nil)))))
(defn mkhook [node]
  (->Hook node #{}))