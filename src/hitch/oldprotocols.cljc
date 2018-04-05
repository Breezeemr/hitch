(ns hitch.oldprotocols
  (:require [hitch.tracking.halt :as halt]
            [hitch.cljc-utils #?(:cljs :refer-macros :default :refer) [defsatisfies]]))

(defonce NOT-FOUND-SENTINEL
  (reify
    #?@(:clj [Object
              (toString [_] "#<NOT-FOUND-SENTINEL>")]
        :cljs
             [IPrintWithWriter
              (-pr-writer [_ writer opts]
                (-write writer "#<NOT-FOUND-SENTINEL>"))])))

(defonce NOT-IN-GRAPH-SENTINEL
  (reify
    #?@(:clj  [Object
               (toString [_] "#<NOT-IN-GRAPH-SENTINEL>")]
        :cljs [IPrintWithWriter
               (-pr-writer [_ writer opts]
                 (-write writer "#<NOT-IN-GRAPH-SENTINEL>"))])))

(defprotocol ISelectorFactory
  (inline
    [selector-factory]
    [selector-factory a]
    [selector-factory a b]
    [selector-factory a b c]
    [selector-factory a b c d]
    [selector-factory a b c d e]
    [selector-factory a b c d e f]
    [selector-factory a b c d e f g]
    [selector-factory a b c d e f g h]))

(defprotocol IDependTrack
  (dget-sel! [this data-selector nf])
  (get-depends [this]))

(defprotocol ITXManager
  (enqueue-dependency-changes [this] "returns removed dependencies"))

(defprotocol IEagerSelectorResolve
  (attempt-eager-selector-resolution! [graph selector nf]
    "Returns the value of the selector if the graph is able to calculate
    it immediately; else returns `nf`."))

(defsatisfies eager-selector-resolve? IEagerSelectorResolve)

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (update-parents [this child add rm])
  (apply-commands [this selector-command-pairs]))

(defprotocol ExternalDependent
  (-change-notify [this]))

;; DEPRECATED!
(defonce berror @#'halt/HALT)
