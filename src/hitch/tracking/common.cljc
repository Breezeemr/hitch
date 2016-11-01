(ns hitch.tracking.common
  (:require [hitch.selector.common :refer [resolution-value]]
            [hitch.protocol.tracking :as ptrack]
            [hitch.protocol.graph :as pgraph]))


(defn simple-tracker [lookupable]
  (let [parents (volatile! #{})]
    (if (satisfies? pgraph/BraidOptimizerProvider lookupable)
      (reify
        ptrack/ILookupTracking
        (-add-depends! [_ selectors]
          (vswap! parents into selectors)
          nil)
        (-depend-sel! [_ unknown selector]
          (vswap! parents conj selector)
          (get lookupable selector unknown))
        (-get-depends [_] @parents)
        pgraph/BraidOptimizerProvider
        (-braid-optimizer [_] nil))
      (reify
        ptrack/ILookupTracking
        (-add-depends! [_ selectors]
          (vswap! parents into selectors)
          nil)
        (-depend-sel! [_ unknown selector]
          (vswap! parents conj selector)
          (get lookupable selector unknown))
        (-get-depends [_] @parents)))))

(defn merge-resolution!
  [tracker unknown resolved|unresolved]
  (ptrack/-add-depends! tracker (:parents resolved|unresolved))
  (resolution-value resolved|unresolved unknown))
