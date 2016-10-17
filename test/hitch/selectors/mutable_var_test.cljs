(ns hitch.selectors.mutable-var-test
  (:require-macros [hitch.eager :refer [go]])
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [devcards.core :as dc :refer-macros [deftest]]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]
            [hitch.graphs.graph :as mgraph]))


(deftest firstt
  (let [graph (mgraph/graph)]
    (let [node1 (graph/hook graph mutable-var :test)]
      (is (= (async/poll! node1) nil))
      (graph/apply-effects graph [[(mutable-var :test) [:set-value 5]]])
      (is (= (async/poll! node1) 5))
      (is (= (async/poll! (graph/hook graph mutable-var :test)) 5)))))

(deftest firstasync
  (let [graph (mgraph/graph)]
    (async done
      (let [testsel (mutable-var :test)]
        (go
          (is (= (async/<! (graph/hook graph  mutable-var :test)) 7))
          (graph/apply-effects graph [[testsel [:clear]]])
          (is (= (async/<! (graph/hook graph  mutable-var :test)) 8))
          (graph/apply-effects graph [[testsel [:clear]]])
          (is (= (async/<! (graph/hook graph  mutable-var :test)) 9))
          (done))
        (graph/apply-effects graph [[testsel [:set-value 7]]])
        (graph/apply-effects graph [[testsel [:set-value 8]]])
        (graph/apply-effects graph [[testsel [:set-value 9]]])))))

(deftest single-hook
         (let [graph (mgraph/graph)]
           (async done
             (let [testsel (mutable-var :test)
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
      (let [testsel (mutable-var :test)]
        (go
          (is (= (async/<! (graph/hook graph mutable-var :test)) 7))
          (is (= (async/<! (graph/hook graph mutable-var :test)) 7))
          (is (= (async/<! (graph/hook graph mutable-var :test)) 7))
          (done)
          )
        (graph/apply-effects graph [[testsel [:set-value 7]]])
        (graph/apply-effects graph [[testsel [:set-value 8]]])
        (graph/apply-effects graph [[testsel [:set-value 9]]])))))