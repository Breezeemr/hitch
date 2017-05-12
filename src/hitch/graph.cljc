(ns hitch.graph
  #?(:clj
     (:import (clojure.lang ILookup)))
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.tracking.halt :as halt :refer [maybe-halt]]))

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


(defn hook-sel
  "Call fn `cb` once with the value of `selector` in `graph` as soon as it is
  available. `cb` may be called synchronously if the selector's value is already
  known."
  [graph cb selector]
  (let [val (let [v (get graph selector oldproto/NOT-IN-GRAPH-SENTINEL)]
              (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
                (if (satisfies? oldproto/IEagerSelectorResolve graph)
                  (oldproto/attempt-eager-selector-resolution! graph selector oldproto/NOT-IN-GRAPH-SENTINEL)
                  oldproto/NOT-IN-GRAPH-SENTINEL)
                v))]
    (if (identical? val oldproto/NOT-IN-GRAPH-SENTINEL)
      (let [h  (mkhook graph selector cb)]
        (oldproto/update-parents graph h #{selector} nil))
      (cb val)))
  nil)

(defn hook-change-sel
  "Call fn `cb` with the value of `selector` in `graph` as soon as it is
  available, and every time the value changes. `cb` may be called synchronously
  if the selector's value is already known.

  Returns a zero-arg unsubscribe function. After it is called, cb will not
  be called again.

  There is no guaranteee that each `cb` call will receive a value not= to the
  previous call's value."
  [graph cb selector]
  (let [val (let [v (get graph selector oldproto/NOT-IN-GRAPH-SENTINEL)]
              (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
                (if (satisfies? oldproto/IEagerSelectorResolve graph)
                  (oldproto/attempt-eager-selector-resolution! graph selector oldproto/NOT-IN-GRAPH-SENTINEL)
                  oldproto/NOT-IN-GRAPH-SENTINEL)
                v))]
    (when-not (identical? val oldproto/NOT-IN-GRAPH-SENTINEL)
      (cb val))
    (let [h (mkhookchange graph selector cb)]
      (oldproto/update-parents graph h #{selector} nil)

      (fn [] (oldproto/update-parents graph h nil #{selector})))))

(defn hook-next-sel
  "Like hook-sel, but will only call `cb` on a change in value
  (edge-triggered). If the selector already has a value, cb will not be called
  until it changes."
  [graph cb selector]
  (let [h (mkhook graph selector cb)]
    (oldproto/update-parents graph h #{selector} nil))
  nil)

(defn dget-sel!
  "Return the value (or `nf` if not yet known) for a selector from graph
  transaction context `tx`."
  [tx selector nf]
  (oldproto/dget-sel! tx selector nf))

(defn hook
  "Call fn `cb` once with the value of selector returned from
  selector-constructor and remaining arguments in `graph` as soon as it is
  available. `cb` may be called synchronously if the selector's value is already
  known."
  ([graph cb selector-constructor] (hook-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-sel graph cb (selector-constructor a b c d f g h))))

(defn hook-change
  "Like hook-change-sel, but receives a selector-constructor plus arguments
  instead of a selector."
  ([graph cb selector-constructor] (hook-change-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-change-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-change-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-change-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-change-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-change-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-change-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-change-sel graph cb (selector-constructor a b c d f g h))))

(defn hook-next
  "Like hook-next-sel, but receives a selector-constructor plus arguments
  instead of a selector."
  ([graph cb selector-constructor] (hook-next-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-next-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-next-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-next-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-next-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-next-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-next-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-next-sel graph cb (selector-constructor a b c d f g h))))

(defn dget!
  "Return the value (or `nf` if not yet known) for a selector-constructor and
  its arguments from graph transaction context `tx`."
  ([tx nf selector-constructor]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor) nf)
     (oldproto/inline selector-constructor tx)))
  ([tx nf selector-constructor a]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor a) nf)
     (oldproto/inline selector-constructor tx a)))
  ([tx nf selector-constructor a b]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor a b) nf)
     (oldproto/inline selector-constructor tx a b)))
  ([tx nf selector-constructor a b c]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor a b c) nf)
     (oldproto/inline selector-constructor tx a b c)))
  ([tx nf selector-constructor a b c d]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor a b c d) nf)
     (oldproto/inline selector-constructor tx a b c d)))
  ([tx nf selector-constructor a b c d e]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor a b c d e) nf)
     (oldproto/inline selector-constructor tx a b c d e)))
  ([tx nf selector-constructor a b c d e f]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor a b c d e f) nf)
     (oldproto/inline selector-constructor tx a b c d e f)))
  ([tx nf selector-constructor a b c d e f g]
   (if *execution-mode*
     (dget-sel! tx (selector-constructor a b c d e f g) nf)
     (oldproto/inline selector-constructor tx a b c d e f g))))

(defn select-sel!
  "Return a box containing the value for a selector from graph transaction
  context `tx`. The returned box is the same as that returned by `select!`."
  ([tx selector]
   (let [v (dget-sel! tx selector oldproto/NOT-IN-GRAPH-SENTINEL)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v)))))

(defn select!
  "Return a box containing the value for a selector-constructor and its
  arguments from graph transaction context `tx`. Retrieve the value with deref
  (@). If the value is not yet known, deref will throw an exception which the
  transaction context will catch. You can test if a value is available
  using `(realized? box)`."
  ([tx selector-constructor]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d e]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d e f]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d e f g]
   (let [v (dget! tx oldproto/NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f g)]
     (if (identical? v oldproto/NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v)))))


(deftype ManualTX
  #?(:cljs
     [graph cb body
      ^:mutable old-requests
      ^:mutable not-requested
      ^:mutable new-requests
      ^:mutable all-requests]
     :clj
     ;; TODO: Not sure how safe these volatiles are in CLJ!
     ;; Or even whether something weaker is ok.
     [graph cb body
      ^:volatile-mutable old-requests
      ^:volatile-mutable not-requested
      ^:volatile-mutable new-requests
      ^:volatile-mutable all-requests])

  #?@(:cljs
      [ILookup
       (-lookup [o data-selector]
         (-lookup o data-selector nil))
       (-lookup [o data-selector not-found]
         (-lookup graph data-selector not-found))]
      :clj
      [ILookup
       (valAt [o data-selector]
         (.valAt o data-selector nil))
       (valAt [o data-selector not-found]
         (.valAt ^ILookup graph data-selector not-found))])

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
        v)))
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
     (halt/maybe-halt
       (body graph)
       oldproto/NOT-FOUND-SENTINEL)))
  ([body a]
   (fn [graph]
     (halt/maybe-halt
       (body graph a)
       oldproto/NOT-FOUND-SENTINEL)))
  ([body a b]
   (fn [graph]
     (halt/maybe-halt
       (body graph a b)
       oldproto/NOT-FOUND-SENTINEL)))
  ([body a b c]
   (fn [graph]
     (halt/maybe-halt
       (body graph a b c)
       oldproto/NOT-FOUND-SENTINEL)))
  ([body a b c d]
   (fn [graph]
     (halt/maybe-halt
       (body graph a b c d)
       oldproto/NOT-FOUND-SENTINEL)))
  ([body a b c d e]
   (fn [graph]
     (halt/maybe-halt
       (body graph a b c d e)
       oldproto/NOT-FOUND-SENTINEL)))
  ([body a b c d e f]
   (fn [graph]
     (halt/maybe-halt
       (body graph a b c d e f)
       oldproto/NOT-FOUND-SENTINEL)))
  ([body a b c d e f g]
   (fn [graph]
     (halt/maybe-halt
       (body graph a b c d e f g)
       oldproto/NOT-FOUND-SENTINEL))))

(defn init-context [graph cb wrapped-body]
  (let [mtx (manual-tx graph cb wrapped-body #{})
        val (binding [*tx-manager* mtx]
              (wrapped-body mtx))]
    (if (identical? val oldproto/NOT-FOUND-SENTINEL)
      (oldproto/enqueue-dependency-changes mtx)
      (cb val)))
  nil)

(defn hitch-callback
  "Given a graph, execute fn `body` in a graph transaction context, calling
  fn `cb` with the result of body when available. `body` will be called like
  `(apply body tx extra-args-to-hitch-callback)`. Body may use select! and
  may be called multiple times.

  If body uses `select!` and derefs unavailable values, the exception will
  be caught and body will be called again when the value is available.
  This continues until body returns without throwing an exception."
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

(defn apply-commands
  "Issue a list of selector-command pairs to a graph. Selector-command-pairs
   is like `[[Selector [command & command-args]] ,,,]`. Do not rely on returned
   value."
  [graph selector-command-pairs]
  (oldproto/apply-commands graph selector-command-pairs))
