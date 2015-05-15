(ns hitch.graph
  (:require [hitch.protocols   :as proto ]
            [hitch.dynamic-dependency :as dyn :include-macros true]))

(declare *default-graph*)

(defn not-loaded? [a]
  (= a ::not-loaded))

(defn- get-val-static [data-selector refs]
  (let [x (into [] (map proto/get-value) refs)]
    (if (some not-loaded? x)
      ::not-loaded
      (proto/get-value! data-selector x))))

(defn- get-val-dynamic [node data-selector refs]
  (dyn/with-dependent node
    (let [val (proto/get-value! data-selector nil)]
      (if (some not-loaded? (map proto/get-value (.-tx-deps refs)))
        ::not-loaded
        val))))

(deftype DependencyNode [data-selector ^:mutable value ^:mutable dependents ^:mutable refs]
  proto/IDependencyNode
  (get-value [_]
    value)
  (resolve-value! [this]    ;-> boolean changed? true if value has changed
    (let [old-value value
          new-value (if (satisfies? proto/IDataSelectorStaticRefs data-selector)
                       (get-val-static data-selector refs)
                       (get-val-dynamic this data-selector refs))]
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
    (let [dependent-ref (.-refs dependent)]
      (when (satisfies? proto/IDependentTransaction dependent-ref)
        (proto/add-dep dependent-ref this)))
    (if (contains? dependents dependent)
      false
      (do (set! dependents (conj dependents dependent))
          true)))
  (un-depend! [_ dependent]                              ;returns last-removed?
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
    (-write writer " :dependents ")
    (pr-writer dependents #_(into #{} (map #(.-selector %)) dependents) writer opts)
    (-write writer " :refs ")
    (pr-writer refs #_(into #{} (map #(.-selector %)) refs) writer opts)
    (-write writer "}")))

(defn resolve-selectors [dependency-graph ref-selectors]
  (into []
        (comp (remove nil?)
              (map (fn [ref]
                     (proto/get-node dependency-graph ref))))
        ref-selectors))

(defn dep-node [dependency-graph data-selector]
  (DependencyNode. data-selector ::not-loaded #{}
                   (cond (satisfies? proto/IDataSelectorStaticRefs data-selector)
                         (resolve-selectors dependency-graph (when (satisfies? proto/IDataSelector data-selector)
                                              (proto/selector-dependencies data-selector)))
                         (satisfies? proto/IDataSelectorDynamicRefs data-selector)
                         (hitch.dynamic-dependency/dependent-transaction))))

;; "deps is a map from graphs => (maps of DataSelectors => DataSelectors state)"
(deftype DependencyGraph [^:mutable nodemap ^:mutable gc-list]
  proto/IDependencyGraph
  (get-node [this data-selector]
    (if-let [n (get nodemap data-selector)]
      n
      (proto/create-node! this data-selector)))
  (create-node! [this data-selector]
    (let [#_(prn "create node for" data-selector)
          new-node (dep-node this data-selector)]
      (set! nodemap  (assoc nodemap data-selector new-node))
      (proto/resolve-value! new-node)
      new-node))
  (gc [this data-selector]
    #_(do (doseq [d (proto/selector-dependencies data-selector)]
          (proto/un-depend! this d))
        true)))

(defn- get-node [dependency-graph data-selector]
  (proto/get-node dependency-graph data-selector))

(defn getn
   ([data-selector]
    (getn *default-graph* data-selector))
   ([dependency-graph data-selector]
    (proto/get-value (proto/get-node dependency-graph data-selector))))


(defn- hitch-node [dependency-graph data-selector dependent]
  (let [n (proto/get-node dependency-graph data-selector)
        new? (when dependent
               (proto/depend! n dependent ))]

    new?                                                      ;Todo
    n))


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
