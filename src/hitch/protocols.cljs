(ns hitch.protocols)

(def ^:dynamic *current-node* nil)

(defonce NOT-LOADED (reify Object
                      (toString [this] "not-loaded")))

(defn not-loaded? [a]
  (or (identical? a NOT-LOADED)
      (and (satisfies? IDeref a)
           (identical? (deref a) NOT-LOADED))))

(def loaded? (complement not-loaded?))

(defprotocol ISubscriber
  (-recalculate! [sub graph]))

(defprotocol ISelectorFactory
  (-selector [selector-factory] [selector-factory a] [selector-factory a b] [selector-factory a b c] [selector-factory a b c d] [selector-factory a b c d e] [selector-factory a b c d e f] [selector-factory a b c d e f g] [selector-factory a b c d e f g h]))

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

(defprotocol InformedSelector
  (dependency-added [dependant-selector dependee-selector ]     ;[dependee-state dependee-effect depender-selector]
                    ;=> new-dependee-effect
                    "Called to inform a selector (the dependee) that another selector (the
                    depender) has started depending on its value. Must return an opaque effect
                    object which the dependee-selector's apply-effect can understand. The
                    returned effect will replace the passed-in dependee-effect.")
  (dependency-removed [dependant-selector dependee-selector ]              ;[dependee-state dependee-effect depender-selector]
                      ;=> new-dependee-effect
                      "Called to inform a selector (the dependee) that another selector (the
                      depender) has ceased depending on its value. Must return an opaque effect
                      object which the dependee-selector's apply-effect can understand. The
                      returned effect will replace the passed-in dependee-effect."
                      ))

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

(defprotocol SelectorValue
  (-value [selector graph state]))



