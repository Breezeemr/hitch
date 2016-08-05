(ns hitch.selectors.kv-store-test
  (:require-macros [hitch.eager :refer [go]]
                   [cljs.core.async.macros]
                   [hitch.selector :refer [defselector]])
  (:require [cljs.test :refer [] :refer-macros [is deftest run-tests async]]
            [hitch.core :as core]
            [hitch.protocols :as proto]
            [hitch.selectors.kv-store :as kv :refer [keyspace key]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]
            [hitch.graphs.mutable :as mgraph]))


(deftest firstt
  (let [graph (mgraph/graph)]
    (proto/clear-graph! graph)
    (async done
      (let [node1 (graph/hook graph key :main :test)
            ks-sel (proto/-selector keyspace :main)]
        (is (= (async/poll! node1) nil))
        (graph/apply-effects graph [[ks-sel [:set-value {:test :cat}]]])
        (go
          (is (= (async/poll! node1) :cat))
          (done))
        ))))

(deftest firstasync
  (let [graph (mgraph/graph)]
    (proto/clear-graph! graph)
    (async done
      (let [node1 (graph/hook graph key :main :test)
            ks-node (graph/hook graph keyspace :main)
            testsel (proto/-selector keyspace :main)        ;(proto/-selector key :main :test)
            ks (async/poll! ks-node)]
        (go
          ;(prn 1)
          (is (= (async/<! node1) 7))
          ;(prn 2)
          (proto/clear-node! node1 graph)
          (is (= (async/<! node1) 8))
          ;(prn 3)
          (proto/clear-node! node1 graph)
          (is (= (async/<! node1) 9))
          ;(prn 4)
          (done))
        (graph/apply-effects graph [[testsel [:set-value {:test 7}]]])
        (graph/apply-effects graph [[testsel [:set-value {:test 8}]]])
        (graph/apply-effects graph [[testsel [:set-value {:test 9}]]])

        ))))

