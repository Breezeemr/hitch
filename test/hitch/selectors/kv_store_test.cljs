(ns hitch.selectors.kv-store-test
  (:require-macros [hitch.eager :refer [go]]
                   [hitch.selector :refer [defselector]]
                   [devcards.core :as dc :refer [deftest]])
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [hitch.core :as core]
            [hitch.mutable.node :as simple]
            [hitch.protocol :as proto]
            [hitch.selectors.kv-store :as kv :refer [keyspace key]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]
            [hitch.mutable.graph :as mgraph]))


(deftest firstt
  (let [graph (mgraph/graph)]
    (async done
      (let [ks-sel (keyspace :main)]
        (is (= (get graph (kv/key ks-sel :test) nil) nil))
        (graph/apply-effects graph [[ks-sel [:set-value {:test :cat}]]])
        (go
          (is (= (async/<! (graph/hook graph kv/key ks-sel :test)) :cat))
          (done))
        ))))

(deftest firstasync
  (let [graph (mgraph/graph)
        testsel (keyspace :main)]
    (is (= (get graph (key testsel :test) :not-found) :not-found))
    (mgraph/get-or-create-node graph (kv/key testsel :test))
    (is (= (get graph (key testsel :test) :not-found) nil))
    (graph/apply-effects graph [[testsel [:set-value {:test 7}]]])
    (is (= (get graph (key testsel :test)) 7))
    (graph/apply-effects graph [[testsel [:set-value {:test 8}]]])
    (is (= (get graph (key testsel :test)) 8))
    (graph/apply-effects graph [[testsel [:set-value {:test 9}]]])
    (is (= (get graph (key testsel :test)) 9))
    ))

