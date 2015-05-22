(ns hitch.graph
  (:require [hitch.protocols   :as proto ]
            [hitch.dynamic-dependency :as dyn :include-macros true]))

(declare *default-graph*)

(defn not-loaded? [a]
  (= a :hitch/not-loaded))


(deftype DependencyNode [data-selector ^:mutable value ^:mutable dependents ^:mutable refs]
  proto/IDependencyNode
  (get-value [_]
    value)
  (resolve-value! [this]    ;-> boolean changed? true if value has changed
    (let [old-value value
          new-value (if (instance? dyn/DependentTransaction refs)
                      (dyn/with-dependent this
                        (let [result (proto/selector-invoke data-selector refs)]
                          (if (proto/selector-ready? data-selector refs)
                            result
                            :hitch/not-loaded)))
                      (if (proto/selector-ready? data-selector refs)
                        (proto/selector-invoke data-selector refs)
                        :hitch/not-loaded))]
      (when (not= new-value old-value)
        (do (set! value new-value)
            true))))
  (invalidate! [this]
    (when (proto/resolve-value! this)
      (run! (fn [d] (when d (proto/invalidate! d))) dependents)))   ; hardcoded to one graph.
  (dependents [_]
    dependents)
  proto/IDependencyTracker
  (depend! [this dependent]                 ;returns new?
    #_(let [dependent-ref (.-refs dependent)]
      (when (satisfies? proto/IDependentTransaction dependent-ref)
        (proto/add-dep dependent-ref this)))
    (if (contains? dependents dependent)
      false
      (do (set! dependents (conj dependents dependent))
          true)))
  (undepend! [_ dependent]                              ;returns last-removed?
    (let [newdeps (disj dependents dependent)]
      (set! dependents newdeps)
      (if (= #{} newdeps)
        (do (proto/gc *default-graph* data-selector)
            true)
        false))
    false)
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#node {:value ")
    (pr-writer value writer opts)
    ;(-write writer " :dependents ")
    ;(pr-writer dependents #_(into #{} (map #(.-selector %)) dependents) writer opts)
    ;(-write writer " :refs ")
    ;(pr-writer refs #_(into #{} (map #(.-selector %)) refs) writer opts)
    (-write writer "}")))

(defn clear-node! [node]
  (set! (.-dependents node) nil)
  (set! (.-refs node) nil))

(defn dep-node [dependency-graph data-selector]
  (DependencyNode. data-selector ::not-loaded #{}
                   (when (satisfies? proto/ISelector data-selector)
                         (proto/selector-init data-selector))))

;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable gc-list]
  proto/IDependencyGraph
  (get-node [this data-selector]
    (get nodemap data-selector)
    )
  (create-node! [this data-selector]
    (let [new-node (dep-node this data-selector)]
      (set! nodemap  (assoc nodemap data-selector new-node))
      (proto/resolve-value! new-node)
      new-node))
  (gc [this data-selector]
    #_(do (doseq [d (proto/selector-dependencies data-selector)]
          (proto/undepend! this d))
        true)))

(defn- get-or-create-node [dependency-graph data-selector]
  (if-let [n (proto/get-node dependency-graph data-selector)]
    n
    (proto/create-node! dependency-graph data-selector)))

(defn getn
   ([data-selector]
    (getn *default-graph* data-selector))
   ([dependency-graph data-selector]
    (if (satisfies? proto/ISelectorSingleton data-selector)
      (proto/get-value (get-or-create-node dependency-graph data-selector))
      (proto/selector-invoke data-selector (proto/selector-init data-selector)))))


(defn hitch-node
  ([data-selector]
    (hitch-node *default-graph* data-selector))
  ([dependency-graph data-selector]
   (if proto/*dependent*
     (hitch-node dependency-graph data-selector proto/*dependent*)
     (get-or-create-node dependency-graph data-selector)))
  ([dependency-graph data-selector dependent]
   (let [n (get-or-create-node dependency-graph data-selector)
         new? (when dependent
                (proto/depend! n dependent))]

     new?                                    ;Todo
     n)))


(defn hitch
  ([data-selector]
   (hitch *default-graph* data-selector proto/*dependent*))
  ([data-selector dependent]
   (hitch *default-graph* data-selector dependent))
  ([dependency-graph data-selector dependent]
   (assert proto/*dependent*)
   (proto/get-value (hitch-node dependency-graph data-selector dependent))))

(defn hitch-get [data-selector]
  (if proto/*dependent*
    (hitch data-selector proto/*dependent* )
    (getn data-selector)))

(defn graph []
  (DependencyGraph. {} []))

(defonce *default-graph* (graph))



(defn clear! [dgraph]
  (doseq [node (vals (.-nodemap dgraph))]
         (clear-node! node))
  (set! (.-nodemap dgraph) {})
  (set! (.-gc-list dgraph) [])
  )

