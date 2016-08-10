(ns hitch.selectors.mutable-var-test
  (:require-macros [hitch.eager :refer [go]]
                   [devcards.core :as dc :refer [deftest]])
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [hitch.protocols :as proto]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]
            [hitch.graphs.mutable :as mgraph]))


(deftest firstt
  (let [graph (mgraph/graph)]
    (let [node1 (graph/hook graph mutable-var :test)]
      (is (= (async/poll! node1) nil))
      (graph/apply-effects graph [[(proto/-selector mutable-var :test) [:set-value 5]]])
      (is (= (async/poll! node1) 5))
      (is (= (async/poll! (graph/hook graph mutable-var :test)) 5)))))

(deftest firstasync
  (let [graph (mgraph/graph)]
    (async done
      (let [node1 (graph/hook graph mutable-var :test)
            testsel (proto/-selector mutable-var :test)]
        (go
          (is (= (async/<! node1) 7))
          (proto/clear-node! node1 graph)
          (is (= (async/<! node1) 8))
          (proto/clear-node! node1 graph)
          (is (= (async/<! node1) 9))
          (done))
        (graph/apply-effects graph [[testsel [:set-value 7]]])
        (graph/apply-effects graph [[testsel [:set-value 8]]])
        (graph/apply-effects graph [[testsel [:set-value 9]]])))))

(deftest take-as-many-as-you-want
  (let [graph (mgraph/graph)]
    (async done
      (let [node1 (graph/hook graph mutable-var :test)
            testsel (proto/-selector mutable-var :test)]
        (go
          (is (= (async/<! node1) 7))
          (is (= (async/<! node1) 7))
          (is (= (async/<! node1) 7))
          (done)
          )
        (graph/apply-effects graph [[testsel [:set-value 7]]])
        (graph/apply-effects graph [[testsel [:set-value 8]]])
        (graph/apply-effects graph [[testsel [:set-value 9]]])))))