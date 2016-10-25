(ns hitch.pin
  (:require [hitch.oldprotocols :as op]
            [hitch.oldprotocols :as oldproto]))

(defonce
  ^{:doc "An inert external dependency. Useful to ensure a selector is not expunged
  from the graph without doing any additional work."}
  PIN
  (reify
    #?@(:clj  [Object
               (toString [_] "#<PIN>")]
        :cljs [IPrintWithWriter
               (-pr-writer [_ writer opts]
                 (-write writer "#<PIN>"))])
    op/ExternalDependent
    (-change-notify [_])))

(defn pin
  "Force a selector to remain in the graph even if nothing else depends on it."
  [depgraph selector]
  (when (satisfies? op/IDependTrack depgraph)
    (oldproto/attempt-eager-selector-resolution! depgraph selector nil))
  (op/update-parents depgraph PIN #{selector} #{}))

(defn unpin
  "Allow a selector to be removed from the graph if nothing else depends on it."
  [depgraph selector]
  (op/update-parents depgraph PIN #{} #{selector}))
