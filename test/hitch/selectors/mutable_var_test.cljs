(ns hitch.selectors.mutable-var-test
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [devcards.core :as dc :refer-macros [deftest]]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as graph]
            [hitch.mutable.graph :as mgraph]
            [hitch.graphs.graph-manager :as gm]
            [hitch.graphs.immutable :as im]))

(def gctor
  mgraph/graph
  #_#(-> (gm/atom-GraphManager (im/->ImmutableGraph 1) gm/nexttick-watcher
         identity)
       :graph-manager gm/dependency-graph-facade))

(deftest firstt
  (let [graph (gctor)]
    (is (= (get graph (mutable-var :test)) nil))
    (graph/apply-commands graph [[(mutable-var :test) [:set-value 5]]])
    (is (= (get graph (mutable-var :test)) 5))))

(deftest firstasync
  (let [graph (gctor)]
    (async done
      (let [testsel (mutable-var :test)
            firstfn (fn [val]
                      (is (= val 7))
                      (graph/hook-change graph
                        (fn [val]

                          (is (= val 8))
                          (graph/hook-change graph
                            (fn [val]
                              (is (= val 9))
                              (done))
                            mutable-var :test)
                          (graph/apply-commands graph [[testsel [:set-value 9]]]))
                        mutable-var :test)
                      (graph/apply-commands graph [[testsel [:set-value 8]]]))]
        (graph/hook graph firstfn mutable-var :test)
        (graph/apply-commands graph [[testsel [:set-value 7]]])))))

(deftest secondasync
         (let [graph (gctor)]
           (async done
             (let [testsel (mutable-var :test)
                   testsel2 (mutable-var :test2)
                   callback (fn [result]
                              (is (= (:a result) 7))
                              (is (= (:b result) 8))
                              (done))
                   firstfn (fn [tx]
                             {:a @(graph/select-sel! tx testsel)
                              :b @(graph/select-sel! tx testsel2)}
                             )]
               (graph/hitch-callback graph callback firstfn)
               (graph/apply-commands graph [[testsel [:set-value 7]]])
               (graph/apply-commands graph [[testsel2 [:set-value 8]]])))))

(deftest single-hook
  (let [graph (gctor)]
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
