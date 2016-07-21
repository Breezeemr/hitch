(ns hitch.selectors.mutable-var-test
  (:require-macros [hitch.eager-go :refer [eager-go]])
  (:require [cljs.test :refer [] :refer-macros [is deftest run-tests async]]
            [hitch.protocols :as proto]
            [hitch.selectors.mutable-var :refer [mutable-var set-var!]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]))


(deftest firstt
  (proto/clear-graph! graph/*graph*)
  (let [node1 (graph/hook graph/*graph* mutable-var :test)]
    (is (= (async/poll! node1) nil))
    (set-var! graph/*graph* :test 5)
    (is (= (async/poll! node1) 5))
    (is (= (async/poll! (graph/hook graph/*graph* mutable-var :test)) 5))))

(deftest firstasync
  (proto/clear-graph! graph/*graph*)
  (async done
    (let [node1 (graph/hook graph/*graph* mutable-var :test)]
      (eager-go
        (is (= (async/<! node1) 7))
        (proto/set-value! node1 nil)
        (is (= (async/<! node1) 8))
        (proto/set-value! node1 nil)
        (is (= (async/<! node1) 9))
        (done)
        )
      (set-var! graph/*graph* :test 7)
      (set-var! graph/*graph* :test 8)
      (set-var! graph/*graph* :test 9))))

(deftest take-as-many-as-you-want
  (proto/clear-graph! graph/*graph*)
  (async done
    (let [node1 (graph/hook graph/*graph* mutable-var :test)]
      (eager-go
        (is (= (async/<! node1) 7))
        (is (= (async/<! node1) 7))
        (is (= (async/<! node1) 7))
        (done)
        )
      (set-var! graph/*graph* :test 7)
      (set-var! graph/*graph* :test 8)
      (set-var! graph/*graph* :test 9))))