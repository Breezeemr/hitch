(ns hitch.tracking.getters
  (:require [hitch.protocol.graph :as pgraph]
            [hitch.tracking.halt :refer [halt? halt! halt-box select-box]]
            [hitch.coil.common :as coil]
            [hitch.util :refer [UNKNOWN unknown?]])
  #?(:clj
     (:import (clojure.lang IDeref IPending))))


(defn dget* [unknown coil tracker args]
  (let [kind (coil/coil-kind coil)]
    (if (= kind :hitch.coil.kind/braid)
      (if (satisfies? pgraph/BraidOptimizerProvider tracker)
        (coil/optimized-invoke-braid! :hitch.coil.context/tracking-getter
          unknown coil tracker args)
        (coil/dget-coil :hitch.coil.context/tracking-getter unknown coil
          tracker args))
      (coil/dget-coil* unknown kind coil tracker args))))

(defn dget [unknown coil tracker & args]
  (dget* unknown coil tracker args))

(defn select* [coil tracker args]
  (let [v (dget* UNKNOWN coil tracker args)]
    (if (unknown? v)
      halt-box
      (select-box v))))

(defn select [coil tracker & args]
  (let [v (dget* UNKNOWN coil tracker args)]
    (if (unknown? v)
      halt-box
      (select-box v))))
