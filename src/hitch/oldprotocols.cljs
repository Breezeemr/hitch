(ns hitch.oldprotocols
  (:require [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as imp-chan]
            [hitch.protocol :as proto]))

(def ^:dynamic *read-mode* false)
(def pending-actions (volatile! []))
(def scheduled-actions (volatile! false))

(defonce NIL-SENTINEL (reify Object
                      (toString [this] "NIL-SENTINEL")
                        IPrintWithWriter
                        (-pr-writer [_ writer opts]
                          (-write writer "#NIL-SENTINEL"))))

(defonce NOT-FOUND-SENTINEL
         (reify Object
           (toString [this] "NOT-FOUND-SENTINEL")
           IPrintWithWriter
           (-pr-writer [_ writer opts]
             (-write writer "#NOT-FOUND-SENTINEL"))))
(defonce NOT-IN-GRAPH-SENTINEL
         (reify Object
           (toString [this] "NOT-IN-GRAPH-SENTINEL")
           IPrintWithWriter
           (-pr-writer [_ writer opts]
             (-write writer "#NOT-IN-GRAPH-SENTINEL"))))

(defn fixnil [v]
  (if (identical? v NIL-SENTINEL)
    nil
    v))
#_(defprotocol ISubscriber
  (-recalculate! [sub graph]))

(defprotocol ISelectorFactory
  (inline [selector-factory] [selector-factory a] [selector-factory a b] [selector-factory a b c] [selector-factory a b c d] [selector-factory a b c d e] [selector-factory a b c d e f] [selector-factory a b c d e f g] [selector-factory a b c d e f g h] ))

(defprotocol IDynamicDepNode
  (get-tx [this])
  (set-tx! [this tx]))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (depend! [this data-selector] "gets node for dataselector")
  (undepend! [this data-selector] "gets node for dataselector")
  (-add-external-dependent [this parent child])
  (-remove-external-dependent [this parent child])
  (-get-external-dependents [this parent]
                            "The current dependencies encountered by this tracker")
  (apply-commands [this selector-command-pairs])
  (create-node! [this data-selector nf]
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

(defprotocol IBatching
  (-request-invalidations [graph invalidations])
  (peek-invalidations [graph])
  (take-invalidations! [graph]))


(defn get-or-effect-graph
  ([graph selector]
   (get-or-effect-graph graph selector NOT-FOUND-SENTINEL))
  ([graph selector nf]
    ;(prn (get graph selector NOT-IN-GRAPH-SENTINEL))
   (let [n (get graph selector NOT-IN-GRAPH-SENTINEL)]
     (if (identical? n NOT-IN-GRAPH-SENTINEL)
       (let [newval (create-node! graph selector NOT-IN-GRAPH-SENTINEL)]
         (if (identical? newval NOT-IN-GRAPH-SENTINEL)
           nf
           newval))
       n)
     )))

(deftype Hook [graph selector ^:mutable handlers]
  ExternalDependent
  (-change-notify [this _ selector-changed]

    (let [val (get graph selector NOT-FOUND-SENTINEL)]
      ;(prn "notify" selector-changed val)
      (when-not (identical? NOT-FOUND-SENTINEL val)
        (-remove-external-dependent graph selector this)
        (run! (fn [handler]
                (let [real-handler (impl/commit handler)]
                  (real-handler val)))
              handlers))))
  impl/ReadPort
  (take! [this ^not-native new-handler]
    (if (not ^boolean (impl/active? new-handler))
      nil
      (let [val (get-or-effect-graph graph selector NOT-FOUND-SENTINEL)]
        ;(prn "take"  val)
        (if (not (identical? val NOT-FOUND-SENTINEL))
          (let [_ (impl/commit new-handler)]
            (imp-chan/box val))
          (do
            (set! handlers (conj handlers new-handler))
            nil))))))
(defn mkhook [graph selector]
  (->Hook graph selector #{}))
