(ns hitch.protocols)

(def ^:dynamic *dependent* )
(defn get-dependent []
  ;(assert *dependent* "dependent should be bound")    must disable check for events
  *dependent*)

(def ^:dynamic *dynamic-dep-tx* nil)



(defprotocol ISelectorSingleton)
(defprotocol ISelectorReload)

(defprotocol ISelector
  (selector-ready? [this graph node] "returns boolean if ready")
  (selector-init [this graph node]"returns a ref object")
  (selector-invoke [this graph node ]"calls selector with a resolved ref object"))

(defprotocol ISelectorNode
  (selector-create-node [this graph]))



(defprotocol IDependentTransaction
  (start [_] "initialize transaction")
  (add-dep [_ dep] "track new dependency")
  (commit [_] "commit transaction"))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (get-node [this data-selector] "gets node for dataselector")
  #_(create-node! [this data-selector]
    "create node and follow init lifecycle")
  (add-node! [this data-selector node]
    "adds node")
  (invalidate-selectors! [graph selectors])
  (clear-graph! [this])
  (gc [this data-selector]
      "Schedule clean up of the resources for dataselector")
  )

(defprotocol IEvaluateRequest
  "evaluate the request possibly building constructing a record"
  (eval-request
    [this parent-node selector-constructor]
    [this parent-node selector-constructor a]
    [this parent-node selector-constructor a b]
    [this parent-node selector-constructor a b c]
    [this parent-node selector-constructor a b c d]
    [this parent-node selector-constructor a b c d f]
    [this parent-node selector-constructor a b c d f g]
    [this parent-node selector-constructor a b c d f g h]))

(defprotocol IGraphlessEval
  "evaluate the request possibly building constructing a record"
  (raw-eval
    [selector-constructor graph parent-node]
    [selector-constructor graph parent-node a]
    [selector-constructor graph parent-node a b]
    [selector-constructor graph parent-node a b c]
    [selector-constructor graph parent-node a b c d]
    [selector-constructor graph parent-node a b c d f]
    [selector-constructor graph parent-node a b c d f g]
    [selector-constructor graph parent-node a b c d f g h]))

(defprotocol ISelectorConstructor
  "evaluate the request possibly building constructing a record"
  (construct-selector
    [selector-constructor]
    [selector-constructor a]
    [selector-constructor a b]
    [selector-constructor a b c]
    [selector-constructor a b c d]
    [selector-constructor a b c d f]
    [selector-constructor a b c d f g]
    [selector-constructor a b c d f g h]))

(defprotocol IDependencyTracker
  "Implemented by function and component caches"
  (depend! [this dependent]
            "Dependency sources call this method if a tracker is bound in the current
             context with dependencies that are encountered during query processing.")
  (undepend! [this dependent])
  (alias! [this dependent])
  (unalias! [this dependent]))

(defprotocol IDependencyNode
  "A utility API for tracking dependencies, allows us to provide more
   advanced options for assembling tracker policies"
  ;(reset! [this] "Clear cache")
  (-get-value [this]
             "Returns cached value if exists for params")
  (set-value! [this new-value]
                  "Informs store that a particular params yeilds value given current store + deps")
  (make-value [this graph] )
  ;(rem-value! [this params])
  #_(invalidate! [this graph changed-node])
  (-dependents [this]
    "The current dependencies encountered by this tracker")
  (-aliased-by [this]
    "The nodes that return this nodes value")
  (-data-selector [this]
    "The nodes that return this nodes value")
  (clear-node! [this graph]))

#_(extend-protocol  IDependencyNode
  default
  (get-value [this] this))

(defn get-value [x]
  (if (satisfies? IDependencyNode x)
    (-get-value x)
    x))
(defn get-dependents [node]
  (-dependents node))
(defn get-aliased-by [node]
  (-aliased-by node))
(defn get-data-selector [node]
  (-data-selector node)
  )

(defprotocol IKeyStore
  "public interface"
  (get-key [this key])
  (set-key [this key value])
  (swap-key! [this fn])
  (clear! [this]))

(defn not-loaded? [a]
  (= (get-value a) :hitch/not-loaded))

(def loaded? (complement not-loaded?))

(defn claim-dependency [graph dependentancy-target dependent]
  (let [dependent-ref (.-refs dependent)]
    (when (and (satisfies? IDependentTransaction dependent-ref) (.-in-tx? dependent-ref))
      (add-dep dependent-ref dependentancy-target)))
  (assert (not= dependent graph))
  ; (assert (or (instance? DependencyNode dependent) (.-props dependent) ) )
  (depend! dependentancy-target dependent)
  )

(defn release-dependency [graph dependentancy-target dependent]
  (when (undepend! dependentancy-target dependent)
    (gc graph (data-selector dependentancy-target)))
  false)

(defn claim-alias [graph alias-node alias-target]
  (alias!  alias-target alias-node)
  )

(defn release-alias [graph alias-node alias-target]
  (when (unalias! alias-target alias-node )
    (gc graph alias-target))
  false)

(defn dnode? [a]
  (satisfies? IDependencyNode a))

(defn make-new-value [graph current-node]
  (make-value current-node graph))



(defn assign-value! [graph node new-value]    ;-> boolean changed? true if value has changed
  (let [old-value (.-value node)]
    (do (when (dnode? old-value)
          (release-alias graph node old-value))
        (when (dnode? new-value)
          (claim-alias graph node old-value))
        (set-value! node new-value)
        true)))

(defn resolve-value! [node graph]    ;-> boolean changed? true if value has changed
  (let [old-value (.-value node)
        new-value (make-new-value graph node)]
    ;(prn "resolve " data-selector new-value old-value)
    (when (not= new-value old-value)
      (assign-value! graph node new-value))))

(defn- get-or-create-node [dependency-graph data-selector dependent]
  (if-let [n (get-node dependency-graph data-selector)]
    (do (when dependent
          (claim-dependency dependency-graph n dependent))
        n)
    (let [n (selector-create-node data-selector dependency-graph)]
      (add-node! dependency-graph data-selector n)
      (binding [hitch.protocols/*dependent* n ]
        (set! (.-refs n) (when (satisfies? ISelector data-selector)
                                  (selector-init data-selector dependency-graph n))))
      (resolve-value! n dependency-graph)
      (when dependent
        (claim-dependency dependency-graph n dependent))
      n)))
