(ns hitch.protocol.tracking)

(defprotocol ChangeAnchors
  (-change-anchor-parents!
    [o anchor add-parents del-parents]
    [o anchor->add+del]
    "Change the parent selectors for one or many anchors.
    Returns ::commands-success or ::command-application-error."))

(defprotocol OptimisticChangeAnchors
  "Allow a transaction to be done speculatively (i.e. in a fork) and the work
  done in it to be efficiently merged back in to the graph if possible."
  ;; Point of this is to allow a tracker to create a "fork", update
  ;; the fork in an auto-commit style, and merge the diff back in to the
  ;; graph. This has two possible advantages:
  ;; 1) Anchor can know its value without waiting for a should-recalc
  ;;    even if the selectors it needs did not exist.
  ;; 2) Even if Anchor value was unresolved, it can avoid doing the extra
  ;;    work twice. (i.e. we made forward progress with fewer recalc cycles)

  ;; However, I am not sure when a merge is safe! Selectors may need to be
  ;; marked "pure" so ensure their dependencies are stable and values
  ;; deterministic. Not sure if we can allow state changes, informs, or effects?
  ;; Also not sure if anchor's parents computed from a speculative tx need to be
  ;; discarded if merge fails.
  (-merge-speculative-anchor-parents!
    [o anchor->add+del graph-basis speculative-selector-nodes]
    [o anchor add-parents del-parents graph-basis speculative-selector-nodes]
    "Attempt to update the graph using the supplied graph-basis and
    speculatively-calculated selector nodes from a speculative transaction.
    The speculative transaction must not have emitted any effects and the
    selector nodes touched must all have deterministic changes.
    If the graph-basis matches, the speculative-selector-nodes are merged
    in without recalculation and the anchor is never notified of the
    transaction. Otherwise the work is discarded and the anchor must
    recalculate."))


;; TODO: This should maybe just be a function the user must write himself
;; and pass around his application.
;; Ideally it is not implemented on a graph-state-manager.
;; Maybe for convenience we make a bundle object which delegates between
;; graph-state-manager and anchor-tracking-provider.
;; specify would be great here in clojure...
(defprotocol AnchorTrackingProvider
  (-make-anchor-tracker [_ graph-state-manager graph-snapshot anchor]
    "Return an AnchorTracker for the provided anchor with reads
    performed against graph-snapshot."))

(defprotocol AnchorTracking
  (-get-anchor [anchor-tracker]
    "Return the anchor whose parents the tracker is tracking.")
  (-get-lookup-tracker [anchor-tracker]
    "Return an ILookupTracking which may optionally implement
    BraidOptimizerProvider.")
  (-enqueue-parent-change! [anchor-tracker add-parents del-parents]
    "Schedule an update of the anchor's parents with the anchor's
    associated graph-state-manager.")
  (-abort! [anchor-tracker]
    "Discard the tracker's state.
    Must also undo any mutations the tracker performed."))

(defprotocol ILookupTracking
  (-add-depends! [tracker selectors]
    "Add to the selectors read.")
  (-depend-sel! [tracker unknown selector]
    "Return a selector's value or unknown if no resolved value, while also
    keeping track of selectors read.")
  (-get-depends [tracker] "Return set of selectors read so far."))

(defprotocol BraidOptimizer
  (-depend-braid! [optimizer context unknown braid lookup-tracker args]
    "Run a braid with the supplied lookup-tracker using an optimal coil,
    return value or unknown if unresolved.")
  (-suggest-coil [opimizer context braid]
    "Find the best-possible implementation of a braid in the given context.
     Return a variant like [:hitch.coil.kind/* implementation] or nil if
     none are preferable

     Possible variants are:

     * :hitch.coil.kind/selector
     * :hitch.coil.kind/selector-provider
     * :hitch.coil.kind/resolver
     * :hitch.coil.kind/haltable"))
