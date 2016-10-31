(ns hitch.protocol.selector)

(defrecord Unresolved [parents])

(defrecord Resolved [value parents])

(defn unresolved? [x]
  (instance? Unresolved x))

(defprotocol Selector
  (-resolution [selector graph-values state]
    "Return a resolution, i.e. Resolved or Unresolved."))

(defn selector? [x]
  (instance? Selector x))

;; StateEffect
;; {:state       STATE
;;  :effects     [EFFECT*]?
;;  :recalculate #{Selector*}?}        ;; Selector must be a direct child
(defrecord StateEffect [state effects recalculate])

(defprotocol StatefulSelector
  (-create [selector] "Return StateEffect. Recalculate must be empty.")
  (-destroy [selector selector-state] "Return an effect."))

(defprotocol SilentSelector)

(defn silent-selector? [s]
  (satisfies? SilentSelector s))

(defprotocol InformedSelector
  (-change-children [selector selector-state add-children del-children]
    "Return StateEffect. May include :recalculate, but members must be direct
    children of the selector."))

(defn informed-selector? [s]
  (satisfies? InformedSelector s))

(defrecord CommandError [msg])

(defn command-error? [x]
  (instance? CommandError x))

(defprotocol CommandableSelector
  (-command-accumulator
    [s state] "Return accumulator.")
  (-command-step [s accumulator command]
    "Return accumulator or CommandError.")
  (-command-result [s accumulator]
    "Return StateEffect or CommandError. May include :recalculate, but members
    must be direct children of the selector."))

(defn stateless-selector-class? [selector-class]
  (and (implements? Selector selector-class)
    (not (or (implements? InformedSelector selector-class)
           (implements? StatefulSelector selector-class)
           (implements? CommandableSelector selector-class)))))

(defn stateless-selector? [selector]
  (and (satisfies? Selector selector)
    (not (or (satisfies? InformedSelector selector)
           (satisfies? StatefulSelector selector)
           (satisfies? CommandableSelector selector)))))

;; ext-children to anchors
;; make basis-t protocol for immutable-graph-value
