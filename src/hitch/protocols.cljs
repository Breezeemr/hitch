(ns hitch.protocols)

(def ^:dynamic *dependent* nil)
(defn get-dependent []
  (or *dependent* (and js/window.quiescent js/quiescent.*component*)))
(def ^:dynamic *dynamic-dep-tx* nil)

(defprotocol ISelectorSingleton)
(defprotocol ISelectorReload)

(defprotocol ISelector
  (selector-ready? [this refs extra] "returns boolean if ready")
  (selector-init [this extra]"returns a ref object")
  (selector-invoke [this refs extra ]"calls selector with a resolved ref object"))

#_(extend-protocol  ISelector
  default
  (selector-ready? [this refs extra] true)
  (selector-init [this extra ] nil)
  (selector-invoke [this refs extra]
    (this)))

(defprotocol IDependentTransaction
  (start [_] "initialize transaction")
  (add-dep [_ dep] "track new dependency")
  (commit [_] "commit transaction"))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (get-node [this data-selector] "gets node for dataselector")
  (create-node! [this data-selector]
            "create node and follow init lifecycle")
  (gc [this data-selector]
      "Schedule clean up of the resources for dataselector")
  )

(defprotocol IDependencyTracker
  "Implemented by function and component caches"
  (depend! [this dependent]
            "Dependency sources call this method if a tracker is bound in the current
             context with dependencies that are encountered during query processing.")
  (undepend! [this dependent]))

(defprotocol IDependencyNode
  "A utility API for tracking dependencies, allows us to provide more
   advanced options for assembling tracker policies"
  ;(reset! [this] "Clear cache")
  (get-value [this]
             "Returns cached value if exists for params")
  (resolve-value! [this]
                  "Informs store that a particular params yeilds value given current store + deps")
  ;(rem-value! [this params])
  (invalidate! [this  changed-node])
  (dependents [this]
                "The current dependencies encountered by this tracker"))

(extend-protocol  IDependencyNode
  default
  (get-value [this] this))

(defprotocol IKeyStore
  "public interface"
  (get-key [this key])
  (set-key [this key value])
  (swap-key! [this fn])
  (clear! [this]))