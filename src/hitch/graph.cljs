(ns hitch.graph
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]))


(def ^:dynamic *execution-mode* true)
(def ^:dynamic *tx-manager* nil)

;(declare apply-effects invalidate-nodes normalize-tx! schedule-actions)
(deftype Hook [graph selector cb]
  oldproto/ExternalDependent
  (-change-notify [this]
    (let [val (get graph selector oldproto/NOT-FOUND-SENTINEL)]
      ;(prn "notify" selector-changed val)
      (when-not (identical? oldproto/NOT-FOUND-SENTINEL val)
        (oldproto/update-parents  graph this nil #{selector})
        (cb val)))))

(defn mkhook [graph selector cb]
  (->Hook graph selector cb))

;(declare apply-effects invalidate-nodes normalize-tx! schedule-actions)
(deftype HookChange [graph selector cb]
  oldproto/ExternalDependent
  (-change-notify [this]
    (let [val (get graph selector oldproto/NOT-FOUND-SENTINEL)]
      ;(prn "notify" selector-changed val)
      (when-not (identical? oldproto/NOT-FOUND-SENTINEL val)
        (cb val)))))

(defn mkhookchange [graph selector cb]
  (->HookChange graph selector cb))


(defn hook-sel [graph cb data-selector]
  (let [val (let [v (get graph data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
              (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
                (if (satisfies? oldproto/IEagerSelectorResolve graph)
                  (oldproto/attempt-eager-selector-resolution! graph data-selector oldproto/NOT-IN-GRAPH-SENTINEL)
                  oldproto/NOT-IN-GRAPH-SENTINEL)
                v))]
    (if (identical? val oldproto/NOT-IN-GRAPH-SENTINEL)
      (let [h  (mkhook graph data-selector cb)]
        (oldproto/update-parents graph h #{data-selector} nil))
      (cb val)))
  nil)

(defn hook-change-sel [graph cb data-selector]
  (let [val (let [v (get graph data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
              (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
                (if (satisfies? oldproto/IEagerSelectorResolve graph)
                  (oldproto/attempt-eager-selector-resolution! graph data-selector oldproto/NOT-IN-GRAPH-SENTINEL)
                  oldproto/NOT-IN-GRAPH-SENTINEL)
                v))]
    (when-not (identical? val oldproto/NOT-IN-GRAPH-SENTINEL)
      (cb val))
    (let [h (mkhookchange graph data-selector cb)]
      (oldproto/update-parents graph h #{data-selector} nil)

      (fn [] (oldproto/update-parents graph h nil #{data-selector})))))

(def dget-sel! oldproto/dget-sel!)

(defn hook
  ([graph cb selector-constructor] (hook-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-sel graph cb (selector-constructor a b c d f g h))))

(defn hook-change
  ([graph cb selector-constructor] (hook-change-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-change-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-change-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-change-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-change-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-change-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-change-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-change-sel graph cb (selector-constructor a b c d f g h))))

(defn dget!
  ([graph nf selector-constructor]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor) nf)
     (oldproto/inline selector-constructor graph)))
  ([graph nf selector-constructor a]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a) nf)
     (oldproto/inline selector-constructor graph a)))
  ([graph nf selector-constructor a b]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b) nf)
     (oldproto/inline selector-constructor graph a b)))
  ([graph nf selector-constructor a b c]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c) nf)
     (oldproto/inline selector-constructor graph a b c)))
  ([graph nf selector-constructor a b c d]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d) nf)
     (oldproto/inline selector-constructor graph a b c d)))
  ([graph nf selector-constructor a b c d e]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d e) nf)
     (oldproto/inline selector-constructor graph a b c d e)))
  ([graph nf selector-constructor a b c d e f ]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d e f) nf)
     (oldproto/inline selector-constructor graph a b c d e f )))
  ([graph nf selector-constructor a b c d e f g ]
   (if *execution-mode*
     (dget-sel! graph (selector-constructor a b c d e f g) nf)
     (oldproto/inline selector-constructor graph a b c d e f g))))

(def bomb (reify
            IDeref
            (-deref [_]
              (throw oldproto/berror))
            IPending
            (-realized? [x]
              false)))
(deftype box [v]
  IDeref
  (-deref [_]
    v)
  IPending
  (-realized? [x]
    true))

(defn select-sel!
  ([graph selector]
   (let [v (dget-sel! graph selector oldproto/NOT-IN-GRAPH-SENTINEL)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v)))))

(defn select!
  ([graph selector-constructor]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e f ]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v))))
  ([graph selector-constructor a b c d e f g ]
   (let [v (dget! graph oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f g)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       bomb
       (->box v)))))


(deftype ManualTX [graph cb body
                       ^:mutable old-requests
                       ^:mutable not-requested
                       ^:mutable new-requests
                       ^:mutable all-requests]
  ILookup
  (-lookup [o data-selector]
    (-lookup o data-selector nil))
  (-lookup [o data-selector not-found]
    (-lookup graph data-selector not-found))
  oldproto/IDependTrack
  (dget-sel! [this data-selector nf]
    (if (old-requests data-selector)
      (set! not-requested (disj! not-requested data-selector))
      (set! new-requests (conj! new-requests data-selector)))
    (set! all-requests (conj! all-requests data-selector))
    (let [v (get this data-selector oldproto/NOT-IN-GRAPH-SENTINEL)]
      (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
        (if (satisfies? oldproto/IEagerSelectorResolve graph)
          (oldproto/attempt-eager-selector-resolution! graph data-selector nf)
          nf)
        v) ))
  (get-depends [this] all-requests)
  oldproto/IDependencyGraph
  (apply-commands [_ selector-command-pairs]
    (oldproto/apply-commands graph selector-command-pairs))
  oldproto/ITXManager
  (enqueue-dependency-changes [this]
    (let [new-deps (persistent! new-requests)
          removed-deps  (persistent! not-requested)
          new-old (reduce disj!
                    (transient (into old-requests new-deps))
                    removed-deps)]
      (set! old-requests (persistent! new-old))
      (set! not-requested (transient old-requests))
      (set! new-requests (transient #{}))
      (set! all-requests (transient #{}))
      (oldproto/update-parents graph this new-deps removed-deps)
      removed-deps))
  oldproto/ExternalDependent
  (-change-notify [this]
    (let [val (binding [*tx-manager* this] (body this))]
      (if (identical? val oldproto/NOT-FOUND-SENTINEL)
        (oldproto/enqueue-dependency-changes this)
        (do
          (oldproto/update-parents graph this nil old-requests)
          (cb val))))))
(defn manual-tx [graph cb body olddeps]
  (->ManualTX graph cb body olddeps (transient olddeps) (transient #{}) (transient #{})))

(defn try-fn
  ([body]
   (fn [graph]
     (try
       (body graph)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex))))))
  ([body a]
   (fn [graph]
     (try
       (body graph a)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex))))))
  ([body a b]
   (fn [graph]
     (try
       (body graph a b)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex))))))
  ([body a b c]
   (fn [graph]
     (try
       (body graph a b c)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex))))))
  ([body a b c d]
   (fn [graph]
     (try
       (body graph a b c d)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex))))))
  ([body a b c d e]
   (fn [graph]
     (try
       (body graph a b c d e)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex))))))
  ([body a b c d e f]
   (fn [graph]
     (try
       (body graph a b c d e f)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex))))))
  ([body a b c d e f g]
   (fn [graph]
     (try
       (body graph a b c d e f g)
       (catch :default ex (if (identical? oldproto/berror ex )
                            oldproto/NOT-FOUND-SENTINEL
                            (throw ex)))))))

(defn init-context [graph cb wrapped-body]
  (let [mtx (manual-tx graph cb wrapped-body #{})
        val (binding [*tx-manager* mtx]
              (wrapped-body mtx))]
    (if (identical? val oldproto/NOT-FOUND-SENTINEL)
      (oldproto/enqueue-dependency-changes mtx)
      (cb val)))
  nil)

(defn hitch-callback
  ([graph cb body]
   (let [wrapped-body (try-fn body)]
     (init-context graph cb wrapped-body)))
  ([graph cb body a]
   (let [wrapped-body (try-fn body a)]
     (init-context graph cb wrapped-body)))
  ([graph cb body a b]
   (let [wrapped-body (try-fn body a b)]
     (init-context graph cb wrapped-body)))
  ([graph cb body a b c]
   (let [wrapped-body (try-fn body a b c)]
     (init-context graph cb wrapped-body)))
  ([graph cb body a b c d]
   (let [wrapped-body (try-fn body a b c d)]
     (init-context graph cb wrapped-body)))
  ([graph cb body a b c d e]
   (let [wrapped-body (try-fn body a b c d e)]
     (init-context graph cb wrapped-body)))
  ([graph cb body a b c d e f]
   (let [wrapped-body (try-fn body a b c d e f)]
     (init-context graph cb wrapped-body)))
  ([graph cb body a b c d e f g]
   (let [wrapped-body (try-fn body a b c d e f g)]
     (init-context graph cb wrapped-body))))

(defn apply-commands [graph selector-command-pairs]
  (oldproto/apply-commands graph selector-command-pairs))
