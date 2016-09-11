(ns hitch.selectors.mutable-var-test
  (:require-macros [hitch.eager :refer [go]])
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [hitch.protocols :as proto]
            [devcards.core :as dc :refer-macros [deftest]]
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
      (let [node1 (graph/get-or-create-node! graph (proto/-selector mutable-var :test))
            testsel (proto/-selector mutable-var :test)]
        (go
          (is (= (async/<! (graph/hook graph  mutable-var :test)) 7))
          (proto/clear-node! node1 graph)
          (is (= (async/<! (graph/hook graph  mutable-var :test)) 8))
          (proto/clear-node! node1 graph)
          (is (= (async/<! (graph/hook graph  mutable-var :test)) 9))
          (done))
        (graph/apply-effects graph [[testsel [:set-value 7]]])
        (graph/apply-effects graph [[testsel [:set-value 8]]])
        (graph/apply-effects graph [[testsel [:set-value 9]]])))))

(deftest single-hook
         (let [graph (mgraph/graph)]
           (async done
             (let [testsel (proto/-selector mutable-var :test)
                   node1 (graph/get-or-create-node! graph testsel)
                   hook (graph/hook graph  mutable-var :test)]
               (go
                 (is (= (async/<! hook) 7))
                 (is (= (async/<! hook) 7))
                 (is (= (async/<! hook) 7))
                 (done))
               (graph/apply-effects graph [[testsel [:set-value 7]]])
               (graph/apply-effects graph [[testsel [:set-value 8]]])
               (graph/apply-effects graph [[testsel [:set-value 9]]])))))

(deftest take-as-many-as-you-want
  (let [graph (mgraph/graph)]
    (async done
      (let [node1 (graph/get-or-create-node! graph (proto/-selector mutable-var :test))
            testsel (proto/-selector mutable-var :test)]
        (go
          (is (= (async/<! (graph/hook graph mutable-var :test)) 7))
          (is (= (async/<! (graph/hook graph mutable-var :test)) 7))
          (is (= (async/<! (graph/hook graph mutable-var :test)) 7))
          (done)
          )
        (graph/apply-effects graph [[testsel [:set-value 7]]])
        (graph/apply-effects graph [[testsel [:set-value 8]]])
        (graph/apply-effects graph [[testsel [:set-value 9]]])))))