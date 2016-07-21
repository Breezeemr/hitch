(ns hitch.selectors.kv-store-test
  (:require-macros [hitch.eager-go :refer [eager-go]]
                   [cljs.core.async.macros :refer [go]]
                   [hitch.selector :refer [def-selector]])
  (:require [cljs.test :refer [] :refer-macros [is deftest run-tests async]]
            [hitch.core :as core]
            [hitch.protocols :as proto]
            [hitch.selectors.kv-store :as kv :refer [keyspace key]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]))


(deftest firstt
  (proto/clear-graph! graph/*graph*)
  (async done
    (let [node1 (graph/hook graph/*graph* key :main :test)
          ks-node (graph/hook graph/*graph* keyspace :main)
          ks (async/poll! ks-node)]
      (is (= (async/poll! node1) nil))

      (swap! ks assoc :test :cat)
      (go
        (is (= (async/poll! node1) :cat))
        (done))
      )))

(deftest firstasync
  (proto/clear-graph! graph/*graph*)
  (async done
    (let [node1 (graph/hook graph/*graph* key :main :test)
          ks-node (graph/hook graph/*graph* keyspace :main)
          ks (async/poll! ks-node)]
      (eager-go
        ;(prn 1)
        (is (= (async/<! node1) 7))
        ;(prn 2)
        (proto/set-value! node1 nil)
        (is (= (async/<! node1) 8))
        ;(prn 3)
        (proto/set-value! node1 nil)
        (is (= (async/<! node1) 9))
        ;(prn 4)
        (done))
      (swap! ks assoc :test 7)
      (swap! ks assoc :test 8)
      (swap! ks assoc :test 9)

      )))

