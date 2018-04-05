(ns hitch.protocol
  "Fundamental selector types and protocols.

  A Selector is a deftype/defrecord whose combination of type and properties
  describes a value. A Selector's value is *calculated* (not *described*)
  from any combination of the following: the Selector's own properties,
  its own (optional) private state, or the value of other Selectors.

  More precisely, a Selector is an immutable, hashable type which implements
  the Selector protocol. Here is a simple example:

      (defrecord Constant [v]
        Selector
        (value [self map-of-selector->value state]
          ;; SelectorValue is a container for value and parents.
          ;; (We will talk about parents later.)
          (->SelectorValue v [])))


      (->Constant 1) ;; A Selector with a constant value of 1, no state, and no
                     ;; parents.

  A Selector may depend on the value of other Selectors for its value; these
  other Selectors are called its Parent Selectors (or just \"parents\").
  If a Selector A depends on another Selector B's value to calculate its own
  value, that Selector B is called A's parent, and A is B's child. (Memonic:
  \"Children depend on their parents for their values.\")

  The Selector protocol provides access to the values of other selectors
  via a map-like object (i.e. something that implements ILookup) from
  Selectors to their values. This selector-value map is passed in as the second
  argument to the Selector protocol's `value` method. The thing that provides
  this map-like object is called a Graph (and is itself a Selector).

  Here is an example of a Selector that depends on the value of other selectors:

      (defrecord Add [parent-selector-a parent-selector-b]
         Selector
         (value [s selector-values state]
           (->SelectorValue
             (+ (get selector-values parent-selector-a 0)
                (get selector-values parent-selector-b 0))
             [])))

      (let [adder  (->Add (->Constant 1) (->Constant 2))
            selval (hitch.protocol/value
                     adder
                     ;; This is the selector-value map.
                     {(->Constant 1) 1, (->Constant 2) 2)}
                     nil)
            ;; selval is a SelectorValue type.
            val    (.-value selval)]
        (assert (= value 3)))

  A Selector may have \"mutable\" private state, but that state must be stored
  outside the selector. A Selector may opt-in to having state by implementing
  the StatefulSelector protocol. Here is a simple example:

      (defrecord RandomInt [max]
        StatefulSelector
        (create [s]
          ;; This is a container for state and effect (a 1-arg side-effecting
          ;; function or nil). We will talk about effects later.
          (->StateEffect (rand-int max) nil nil))
        (destroy [s state]
          ;; You may return an effect here, but this Selector doesn't need it.
          nil)

        Selector
        (value [this selector-value-map state]
          (->SelectorValue state)))

  The only permissible way to alter a Selector's state is via Commands.
  An Command is a value, interpreted by the Selector type, describing a change
  to a Selector's state. A set of Commands are applied to Selectors
  transactionally.

  A Selector can opt-in to receiving Commands via the CommandableSelector
  protocol, which is responsible for receiving the Commands and returning the
  new state.

  The CommandableSelector protocol has the shape of a reducing function. It has
  three methods:

  * command-accumulator: Given the old state, return an accumulator for a
    reductive command-application process.
  * command-step: Given the accumulator and one command, return a new
    accumulator value. This value will be passed to the next invocation of
    command-step.
  * command-result: Called after all Commands are processed.
    Takes the accumulator and returns a new state value and an effect-function
    for side-effects. (We'll talk about effect functions later.)

  Here is an example of an CommandableSelector:

      ;; No properties
      (defrecord Variable []
        Selector
        (value [s sv state] state)

        CommandableSelector
        (command-accumulator [s old-state] old-state)
        (command-step [s accumulator command]
          (case (first command)
            :set! (second command)))
        (command-result [s accumulator]
          ;; StateEffect is another container which holds state and effect.
          (->StateEffect accumulator nil nil)))

      (let [var       (->Variable)
            old-state nil
            command    [:set! 2]
            ;; initialize
            acc       (command-accumulator var old-state)
            ;; feed commands
            acc       (command-step var acc command)
            ;; collect result
            result    (command-result var acc)
            new-state (.-state result)
            new-value (.-value (value var {} new-state))]
        (assert (= new-value 2))



  ;; TODO Graph
  ;; TODO transactions
  ;; TODO Effects


  Some terminology:

  * Parent selector: A \"requires\" relationship with another Selector. If A
    requires B, B is the parent of A.
  * Child selector: A \"required-by\" relationship with another Selector.
    If A requires B, A is the child of B.
  * Selector value: The value of a Selector; must be determined entirely from
    the value of other Selectors (i.e., its parents) or from the Selector's
    own private state.
  * Selector state: an object private to a selector which may be changed by
    commands.
  * Command: Data describing a change to a selector's state. Consumed by the
    CommandableSelector protocol.
  * Effect: A 1-arg function which, when called, performs some side-effect.
    Effects are only called at the end of a successful transaction after all
    commands have been processed and new selector values recalculated.
    The effect function receives a transactable GraphManager as its argument."
  (:refer-clojure :exclude [Var])
  #?(:cljs
     (:require-macros [hitch.protocol :refer [if-clj-target]])
     :clj
     (:import (java.util.concurrent ConcurrentHashMap))))


(defmacro if-clj-target [clj-body cljs-body]
  #?(:clj
     (if (contains? &env '&env)
       `(if (:ns ~'&env) ~cljs-body ~clj-body)
       (if (:ns &env) cljs-body clj-body))
     :cljs
     cljs-body))


(defrecord SelectorUnresolved [parents])

(defn selector-unresolved? [x]
  (instance? SelectorUnresolved x))

(defrecord SelectorValue [value parents])

(defprotocol Selector
  "Return a value and selectors read out of `selector-values`
  and selector's private `state` (or nil if it has no state).
  Must return `SelectorValue` if the selector was able to determine a value, or
  `SelectorUnresolved` if it cannot calculate its value from the other
  values available in `selector-values`. In all cases it must return a set
  of selectors it requires to calcualte its value. When any of these selectors
  update, this method is rerun to calculate the new value."
  (value [s selector-values state]
    "Return a SelectorValue or SelectorUnresolved."))

(defprotocol StatefulSelector
  "Private state, initializer, and finalizer for Selectors."
  (create [s]
    "Return a `StateEffect` with an opaque value for this Selector's
    use in the Selector or CommandableSelector protocols, and an effect
    (1-arg fn accepting a transactable GraphManager) to initialize the service.
    The effect may be `nil` if no effect function is needed.
    This `create` method must be pure: put side-effects in the effect.")
  (destroy [s state]
    "Return an effect or nil to clean up after this selector's destruction.
    This method must be pure: put side-commands in the effect."))

(defprotocol SilentSelector
  "If this protocol is present, child selectors' values are *not* recalculated
  when this selector changes. Instead, this selector must mark them stale in
  the return value of command-result.")

(defprotocol InformedSelector
  "A marker protocol. When present, :hitch.protocol/child-add and child-del
  commands are added to the selector's command queue which inform when child
  selectors begin or cease depending on the current selector.")

(defrecord StateEffect [state effect recalc-child-selectors])

(defrecord CommandError [accumulator pending-commands bad-command error])

(defn command-error? [x]
  (instance? CommandError x))

(defprotocol CommandableSelector
  "Allows a selector to receive commands and alter its state based on commands.
  These methods are only called if a selector has commands in the current
  transaction."
  (command-accumulator
    [s state] "Return an opaque accumulator for command-step. `state` will be
    nil if it has never been initialized via StatefulSelector or a previous
    command-result call.")
  (command-step [s accumulator command]
    "Accept a command and return a new accumulator. This method is called once
    per command.

    At any time this function may return a CommandError which the caller may
    use to determine how to handle the error. It should be possible in principle
    (although not necessarily in practice) to resume the command reduction
    process using the data in a CommandError. :accumulator, :pending-commands,
    and :bad-command will be added by the caller of this method, implementers
    only need to supply :error.")
  (command-result [s accumulator]
    "Return an StateEffect after all commands have been processed, which
    contains the new state and an effect.

    Only if this selector also implements SilentSelector may it include a
    vector of children to recalculate using the :recalc-child-selectors key
    of StateEffect."))


(defprotocol Var
  (machine-selector [v]
    "Return the selector of a machine which will implement this var.
  Also automatically makes the var a child of the machine."))

(defprotocol Machine
  (apply-machine-commands [m read-only-graph node-state commands]
    "Advance the state of the machine and graph in response to commands.

    Receives:

    * `read-only-graph` ILookup from selector key to current (possibly
      uncommitted) selector value.
    * `node-state` State of the machine's node in the graph. Has the following
      keys:
      - `:state` The machine's private state.
      - `:children` A set of selectors (all Vars) which depend on this machine.
      - `:parents` A set of selectors which this machine depends on.
        Whenever the value of a parent selector changes, the machine will
        receive a `[:hitch.protocol/parent-value-change parent-selector]`
        command.
    * `commands` A reducible collection of commands. Possible commands are:
      - `[:hitch.protocol/parent-value-change parent-selector]` Issued when
        a parent selector's value changed.
      - `[:hitch.protocol/child-add var-selector]` Issued when a child var
        begins depending on the machine.
      - `[:hitch.protocol/child-del var-selector]` Issued a child var ceases
        depending on the machine.
      - `[:hitch.protocol/var-command var-selector command]` Issued when a var
        received a command. All commands issued to vars are forwarded to their
        machine. It is not possible to issue a command to a machine *except*
        through one of its vars.


    Returns a map with the following keys:

    * `:state` The new private state of the machine.
    * `:dep-change` A map from a selector to a boolean. `true` means add the
      selector as a machine parent and inform the machine of changes to the
      parent's value; `false` means remove the selector as a machine parent.
      It is ok to depend on your own vars: this is the only way to
      guarantee that the var will exist. Dependency changes are applied
      before `:var-reset`.
    * `:var-reset` A map of var selectors (which must be machine-controlled!)
       to a new value for each var. Vars with no children will be ignored.
    * `:effect` An optional effect function."))

(defprotocol ExternalDependent2
  (-change-notify2 [_ g-snapshot parent-changes]
    "Notify an external dependent that some of its dependencies' values have
    changed. It receives a shapshot of the graph and a set of selectors which
    changed. (To retrieve the actual new value, look up the selectors in the
    graph snapshot."))

(defprotocol GraphManager
  (transact! [graph-manager cmds]
    "Apply a transaction to a graph manager and mutate the graph.

     This operation should mutate the graph manager so it contains the result
     of its internal graph-node after applying the commands. The graph manager
     must also ensure that any effects from the operation are executed and that
     any external-children are recalculated.

     It must return either:

         [:hitch.protocol/tx-ok {:value-before             old-sel->value-map
                                 :value-after              new-sel->value-map}]

         [:hitch.protocol/tx-error hitch.protocol/CommandError-instance]"))


;; This is to get around satisfies? being slow in CLJ until
;; https://dev.clojure.org/jira/browse/CLJ-1814 is applied
;; CLJS is already fast
(if-clj-target
  (defn- fast-satisfies [protocol-var]
    ;; copied and modified from manifold.utils/fast-satisfies
    (let [classes (ConcurrentHashMap. 16 0.9 1)]
      (add-watch protocol-var ::memoization
        (fn [_ _ _ _] (.clear classes)))
      (fn [^Object x]
        (let [protocol @protocol-var]
          (if (nil? x)
            (contains? (:impls protocol) nil)
            (if (instance? (:on-interface protocol) x)
              true
              (let [cls (.getClass x)]
                (if-some [s? (.get classes cls)]
                  s?
                  (.putIfAbsent classes cls (satisfies? protocol x))))))))))
  nil)

(if-clj-target
  (def stateful-selector?
    (fast-satisfies #'StatefulSelector))
  (defn ^boolean stateful-selector? [x]
    (satisfies? StatefulSelector x)))

(if-clj-target
  (def silent-selector?
    (fast-satisfies #'SilentSelector))
  (defn ^boolean silent-selector? [x]
    (satisfies? SilentSelector x)))

(if-clj-target
  (def informed-selector?
    (fast-satisfies #'InformedSelector))
  (defn ^boolean informed-selector? [x]
    (satisfies? InformedSelector x)))

(if-clj-target
  (def machine-selector?
    (fast-satisfies #'Machine))
  (defn ^boolean machine-selector? [x]
    (satisfies? Machine x)))

(if-clj-target
  (def var-selector?
    (fast-satisfies #'Var))
  (defn ^boolean var-selector? [x]
    (satisfies? Var x)))

(if-clj-target
  (def external-dependent2?
    (fast-satisfies #'ExternalDependent2))
  (defn ^boolean external-dependent2? [x]
    (satisfies? ExternalDependent2 x)))
