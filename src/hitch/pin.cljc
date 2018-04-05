(ns hitch.pin
  (:require [hitch.oldprotocols :as op])
  #?(:clj
     (:import (java.io Writer))))

(defonce
  ^{:doc "An inert external dependency. Useful to ensure a selector is not expunged
  from the graph without doing any additional work."}
  PIN
  (->
    (reify
      #?@(:clj  [Object
                 (toString [_] "#<PIN>")]
          :cljs [IPrintWithWriter
                 (-pr-writer [_ writer opts]
                   (-write writer "#<PIN>"))])
      op/ExternalDependent
      (-change-notify [_]))
    #?(:clj (vary-meta assoc :type ::PIN))))

#?(:clj
   (defmethod print-method ::PIN [c, ^Writer w]
     (.write w "#<PIN>")))

(defn pin
  "Force a selector to remain in the graph even if nothing else depends on it."
  [depgraph selector]
  (when (op/eager-selector-resolve? depgraph)
    (op/attempt-eager-selector-resolution! depgraph selector nil))
  (op/update-parents depgraph PIN #{selector} #{}))

(defn unpin
  "Allow a selector to be removed from the graph if nothing else depends on it."
  [depgraph selector]
  (op/update-parents depgraph PIN #{} #{selector}))
