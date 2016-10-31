(ns hitch.protocol.graph)

;; ## Defined Variants

;; ::command-application-error
;; {:selector         Selector
;;  :error            CommandError
;;  :state            STATE        ;; Selector's old state
;;  :stage            :command-step | :command-accumulator | :command-result
;;  :accumulator      ACCUMULATOR?
;;  :bad-command      COMMAND?
;;  :pending-commands [COMMAND*]}

;; ::commands-success
;; {:value-before ILookup<Selector,ANY?>
;;  :value-after  ILookup<Selector,ANY?>
;;  :effects     [EFFECT-FN*]
;;  :recalculate [ANCHOR*]
;;
;;  ;; Maybe this too?
;;  ;; Selector key not present if no changes
;;  ;; i.e. one of the keys in the value-map must be present.
;;  :changes     {(Selector {:version   0..*         ;; incremented whenever a selector node changes (or is touched?)
;;                (:value    [before after])?
;;                (:state    [before after])?
;;                (:children [before after])?
;;                (:anchors  [before after])?
;;                (:parents  [before after])?})*}
;;  }

(defprotocol GraphStateManager
  (-get-snapshot [graph-state-manager]
    "Return a snapshot of a graph's last known values. (Snapshot not guaranteed
    to be immutable--depends on the graph implementation.)
    Snapshot implements ILookup from Selector to the selector's last known
    value. May also implement ImmutableGraphSnapshot if the snapshot is truly
    an immutable snapshot, or BraidOptimizerProvider if it can optimize coils.")
  (-apply-commands! [graph-state-manager graph-commands]
    "Return a deref containing either ::commands-success or
    ::command-application-error. Work may be done asynchronously."))

(defprotocol GraphBasis
  (-graph-basis [graph-snapshot]
    "Return the basis of a graph-values snapshot, i.e. the graph's
    selector and t-point: e.g., {:selector #Immutable{} :t 10}"))

(defprotocol SelectorBasis
  (-selector-basis [graph-snapshot selector]
    "Return the basis of a particular selector, or nil if the selector does not
    yet exist in the graph.
    The basis of a selector increments every time its value or state changes
    within a transaction."))

(defprotocol BraidOptimizerProvider
  (-braid-optimizer [this] "Return associated BraidOptimizer, if available."))
