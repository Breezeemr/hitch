(ns hitch.eager
  (:require [cljs.core.async :as async]))

(defn promise-all
  "Given a collection of takeable items, return a promise channel which will
  contain a vector of the next available value taken from every item, preserving
  order if it exists in the original collection. If any takeable is closed
  the promise will be closed.

  Runs as synchronously as possible using poll! and offer!, only becoming async
  if one of the takeable items is not immediately takeable.

  Example:

      (go
        (let [a (chan)
              b (chan)]
          (>! a 1)
          (>! b 2)
          (= [1 2] (<! (promise-all [a b])))))"
  [takeables]
  (let [p       (async/promise-chan)
        nc      (count takeables)
        waiting (volatile! nc)
        a       (object-array nc)]
    (run! (map-indexed
            (fn [i takeable]
              (if-some [v (async/poll! takeable)]
                (do
                  (aset a i v)
                  (vswap! waiting dec))
                (async/take! takeable
                             (fn [v]
                               (if (nil? v)
                                 (async/close! p)
                                 (do
                                   (aset a i v)
                                   (when (zero? (vswap! waiting dec))
                                     (async/offer! p (into [] a))))))))))
          takeables)
    (when (zero? @waiting)
      (async/offer! p (into [] a)))
    p))
