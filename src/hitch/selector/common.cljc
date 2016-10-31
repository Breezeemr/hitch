(ns hitch.selector.common
  (:require [hitch.protocol.selector :as psel]
            [hitch.util :refer [UNKNOWN unknown?]]))


(defn resolution-value [resolved|unresolved unknown]
  (if (psel/unresolved? resolved|unresolved)
    unknown
    (:value resolved|unresolved)))

(defn get-selector-resolution [lookupable selector]
  (let [v (get lookupable selector UNKNOWN)]
    (if (unknown? v)
      (psel/->Unresolved #{selector})
      (psel/->Resolved v #{selector}))))
