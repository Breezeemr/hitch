(ns hitch.oldprotocols
  (:require [hitch.protocol :as proto]))


(defonce NOT-FOUND-SENTINEL
         (reify Object
           (toString [this] "NOT-FOUND-SENTINEL")
           IPrintWithWriter
           (-pr-writer [_ writer opts]
             (-write writer "#NOT-FOUND-SENTINEL"))))
(defonce NOT-IN-GRAPH-SENTINEL
         (reify Object
           (toString [this] "NOT-IN-GRAPH-SENTINEL")
           IPrintWithWriter
           (-pr-writer [_ writer opts]
             (-write writer "#NOT-IN-GRAPH-SENTINEL"))))

(defprotocol ISelectorFactory
  (inline [selector-factory] [selector-factory a] [selector-factory a b] [selector-factory a b c] [selector-factory a b c d] [selector-factory a b c d e] [selector-factory a b c d e f] [selector-factory a b c d e f g] [selector-factory a b c d e f g h] ))

(defprotocol IDependTrack
  (dget-sel! [this data-selector nf])
  (get-depends [this]))

(defprotocol ITXManager
  (enqueue-dependency-changes [this] "returns removed dependencies"))

(defprotocol IEagerSelectorResolve
  (attempt-eager-selector-resolution! [this parent nf]))

(extend-protocol IEagerSelectorResolve
  default
  (attempt-eager-selector-resolution! [this parent nf] nf))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (update-parents [this child add rm ])
  (apply-commands [this selector-command-pairs]))

(defprotocol ExternalDependent
  (-change-notify [this]))


