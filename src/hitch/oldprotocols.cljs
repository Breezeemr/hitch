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
  (depend! [this data-selector])
  (get-depends [this]))

(defprotocol ITXManager
  (enqueue-dependency-changes [this] "returns removed dependencies"))

(defprotocol IEagerDepend
  (eager-depend! [this child parent]))

(defprotocol IDependencyGraph
  "Implemented by function and component caches"
  (update-parents [this child add rm ])
  (-get-external-dependents [this parent]
                            "The current dependencies encountered by this tracker")
  (apply-commands [this selector-command-pairs])
  (create-node! [this data-selector nf]
                "adds node")
  (clear-graph! [this])
  (gc [this data-selector]
      "Schedule clean up of the resources for dataselector"))

(defprotocol ExternalDependent
  (-change-notify [this]))



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


