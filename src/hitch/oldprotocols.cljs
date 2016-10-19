(ns hitch.oldprotocols
  (:require [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as imp-chan]
            [hitch.protocol :as proto]))


(defonce NIL-SENTINEL (reify Object
                      (toString [this] "NIL-SENTINEL")
                        IPrintWithWriter
                        (-pr-writer [_ writer opts]
                          (-write writer "#NIL-SENTINEL"))))

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

(defn fixnil [v]
  (if (identical? v NIL-SENTINEL)
    nil
    v))

(defprotocol ISelectorFactory
  (inline [selector-factory] [selector-factory a] [selector-factory a b] [selector-factory a b c] [selector-factory a b c d] [selector-factory a b c d e] [selector-factory a b c d e f] [selector-factory a b c d e f g] [selector-factory a b c d e f g h] ))

(defprotocol IDependTrack
  (depend! [this data-selector])
  (get-depends [this]))

(defprotocol ITXManager
  (apply-tx! [this] "returns removed dependencies"))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (-add-external-dependent [this parent child])
  (-remove-external-dependent [this parent child])
  (-get-external-dependents [this parent]
                            "The current dependencies encountered by this tracker")
  (apply-commands [this selector-command-pairs])
  (create-node! [this data-selector nf]
                "adds node")
  (clear-graph! [this])
  (gc [this data-selector]
      "Schedule clean up of the resources for dataselector"))

(defprotocol ExternalDependent
  (-change-notify [this graph selector-changed]))



(defprotocol IBatching
  (-request-invalidations [graph invalidations])
  (peek-invalidations [graph])
  (take-invalidations! [graph]))


(defn get-or-effect-graph
  ([graph selector]
   (get-or-effect-graph graph selector NOT-FOUND-SENTINEL))
  ([graph selector nf]
    ;(prn (get graph selector NOT-IN-GRAPH-SENTINEL))
   (let [n (get graph selector NOT-IN-GRAPH-SENTINEL)]
     (if (identical? n NOT-IN-GRAPH-SENTINEL)
       (let [newval (create-node! graph selector NOT-IN-GRAPH-SENTINEL)]
         (if (identical? newval NOT-IN-GRAPH-SENTINEL)
           nf
           newval))
       n)
     )))


