(ns hitch.protocols)

(defonce NOT-LOADED (reify Object
                      (toString [this] "not-loaded")))

(defn not-loaded? [a]
  (or (identical? a NOT-LOADED)
      (and (satisfies? IDeref a)
           (identical? (deref a) NOT-LOADED))))

(def loaded? (complement not-loaded?))

(defprotocol ISubscriber
  (-active? [sub graph])
  (-recalculate! [sub graph]))

(defprotocol IInternalSubscriber
  (-internal? [sub graph]))

(extend-protocol IInternalSubscriber
  default
  (-internal? [sub graph] false))

(defprotocol ISelector
  (-eval [selector graph] [selector graph a] [selector graph a b] [selector graph a b c] [selector graph a b c d] [selector graph a b c d e] [selector graph a b c d e f] [selector graph a b c d e f g] [selector graph a b c d e f g h])
  (-selector [selector graph] [selector graph a] [selector graph a b] [selector graph a b c] [selector graph a b c d] [selector graph a b c d e] [selector graph a b c d e f] [selector graph a b c d e f g] [selector graph a b c d e f g h]))

(defprotocol ICreateNode
  (-create-node [this graph]))

(defprotocol IDynamicDepNode
  (get-tx [this])
  (set-tx! [this tx]))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (get-node [this data-selector] "gets node for dataselector")
  #_(create-node! [this data-selector]
                  "create node and follow init lifecycle")
  (add-node! [this data-selector node]
             "adds node")
  (clear-graph! [this])
  (gc [this data-selector]
      "Schedule clean up of the resources for dataselector"))

(defprotocol IDependencyTracker
  "Implemented by function and component caches"
  (depend! [graph dependee dependent]
           "Dependency sources call this method if a tracker is bound in the current
            context with dependencies that are encountered during query processing.")
  (undepend! [graph dependee dependent]))

(defprotocol INodeDependencyTracker
  "Implemented by function and component caches"
  (node-depend! [dependee dependent]
                "Dependency sources call this method if a tracker is bound in the current
                 context with dependencies that are encountered during query processing.")
  (node-undepend! [dependee dependent]))

(defprotocol IDependencyNode
  "A utility API for tracking dependencies, allows us to provide more
   advanced options for assembling tracker policies"
  (-get-value [this]
              "Returns cached value if exists for params")
  (set-value! [this new-value]
              "Informs store that a particular params yeilds value given current store + deps")
  (-dependents [this]
               "The current dependencies encountered by this tracker")
  (-take-one-time-dependents! [this]
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


