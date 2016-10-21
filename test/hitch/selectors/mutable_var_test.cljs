(ns hitch.selectors.mutable-var-test
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [devcards.core :as dc :refer-macros [deftest]]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]
            [hitch.mutable.graph :as mgraph]))


(deftest firstt
  (let [graph (mgraph/graph)]
    (is (= (get graph (mutable-var :test)) nil))
    (graph/apply-commands graph [[(mutable-var :test) [:set-value 5]]])
    (is (= (get graph (mutable-var :test)) 5))))

(deftest firstasync
         (let [graph (mgraph/graph)]
           (async done
             (let [testsel (mutable-var :test)
            firstfn (fn [val]
                      (is (= val 7))
                      (graph/apply-commands graph [[testsel [:clear]]])
                      (graph/hook graph
                                  (fn [val]
                                    (is (= val 8))
                                    (graph/apply-commands graph [[testsel [:clear]]])
                                    (graph/hook graph
                                                (fn [val]
                                                  (is (= val 9))
                                                  (done))
                                                mutable-var :test)
                                    (graph/apply-commands graph [[testsel [:set-value 9]]]))
                                  mutable-var :test)
                      (graph/apply-commands graph [[testsel [:set-value 8]]]))]
               (graph/hook graph firstfn
                    mutable-var :test)
               (graph/apply-commands graph [[testsel [:set-value 7]]])))))

(deftest single-hook
         (let [graph (mgraph/graph)]
           (async done
             (let [testsel (mutable-var :test)]
               (graph/hook graph
                           (fn [val]
                             (is (= val 7)))
                           mutable-var :test)
               (graph/hook graph
                           (fn [val]
                             (is (= val 7)))
                           mutable-var :test)
               (graph/hook graph
                           (fn [val]
                             (is (= val 7)))
                           mutable-var :test)
               (graph/apply-commands graph [[testsel [:set-value 7]]])
               (graph/apply-commands graph [[testsel [:set-value 8]]])
               (graph/apply-commands graph [[testsel [:set-value 9]]])
               (done)))))
