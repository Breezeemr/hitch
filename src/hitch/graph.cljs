(ns hitch.graph
  (:require [hitch.protocols   :as proto ]
            [hitch.dynamic-dependency :as dyn :include-macros true]))

(declare *default-graph* DependencyNode)

(defn not-loaded? [a]
  (= (proto/get-value a) :hitch/not-loaded))

(def loaded? (complement not-loaded?))

(defn dnode? [a]
  (instance? DependencyNode a))
(deftype DependencyNode [data-selector ^:mutable value ^:mutable dependents ^:mutable refs]
  proto/IDependencyNode
  (get-value [_]
    (proto/get-value value))                                 ;unwrap references
  (resolve-value! [this]    ;-> boolean changed? true if value has changed
    (let [old-value value
          new-value (if (instance? dyn/DependentTransaction refs)
                      (dyn/with-dependent this
                        (let [result (proto/selector-invoke data-selector refs nil)]
                          (if (proto/selector-ready? data-selector refs  nil)
                            result
                            :hitch/not-loaded)))
                      (if (proto/selector-ready? data-selector refs  nil)
                        (proto/selector-invoke data-selector refs  nil)
                        :hitch/not-loaded))]
      (when (not= new-value old-value)
        (do (when (dnode? old-value)
              (proto/undepend! this old-value))
            (when (dnode? new-value)
              (proto/depend! this new-value))
            (set! value new-value)
            true))))
  (invalidate! [this changed-node]
    (when (or (and (identical? changed-node value) (not (nil? changed-node))) (proto/resolve-value! this))
      (run! (fn [d] (when d (proto/invalidate! d this))) dependents)))   ; hardcoded to one graph.
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
  (undepend! [this dependent]                              ;returns last-removed?
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
  (when (dnode? (.-value node))
    (proto/undepend! node (.-value node)))
  (set! (.-dependents node) nil)
  (set! (.-refs node) nil))

(defn dep-node [dependency-graph data-selector extra]
  (DependencyNode. data-selector :hitch/not-loaded #{} nil))

;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable gc-list]
  proto/IDependencyGraph
  (get-node [this data-selector]
    (get nodemap data-selector))
  (create-node! [this data-selector]
    (let [new-node (dep-node this data-selector nil)]
      (set! nodemap  (assoc nodemap data-selector new-node))
      (set! (.-refs new-node)  (when (satisfies? proto/ISelector data-selector)
                                (proto/selector-init data-selector nil)))
      new-node))
  (gc [this data-selector]
    #_(do (doseq [d (proto/selector-dependencies data-selector)]
          (proto/undepend! this d))
        true)))

(defn- get-or-create-node [dependency-graph data-selector dependent]
  (if-let [n (proto/get-node dependency-graph data-selector)]
    (do (when dependent
          (proto/depend! n dependent))
      n)
    (let [n (proto/create-node! dependency-graph data-selector)]
      (when dependent
        (proto/depend! n dependent))
      (proto/invalidate! n nil)
      n)))

(defn getn
   ([data-selector]
    (getn *default-graph* data-selector))
   ([dependency-graph data-selector]
    (if (satisfies? proto/ISelectorSingleton data-selector)
      (proto/get-value (get-or-create-node dependency-graph data-selector nil ))
      (proto/selector-invoke data-selector (proto/selector-init data-selector nil) nil))))


(defn hitch-node
  ([data-selector]
    (hitch-node *default-graph* data-selector))
  ([dependency-graph data-selector]
   (if (proto/get-dependent)
     (hitch-node dependency-graph data-selector (proto/get-dependent))
     (get-or-create-node dependency-graph data-selector nil)))
  ([dependency-graph data-selector dependent]
   (get-or-create-node dependency-graph data-selector dependent)))


(defn hitch
  ([data-selector]
   (hitch *default-graph* data-selector (proto/get-dependent)))
  ([data-selector dependent]
   (hitch *default-graph* data-selector dependent))
  ([dependency-graph data-selector dependent]
   (assert (proto/get-dependent))
   (proto/get-value (hitch-node dependency-graph data-selector dependent))))

(defn hitch-get [data-selector]
  (if (proto/get-dependent)
    (hitch data-selector (proto/get-dependent) )
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

