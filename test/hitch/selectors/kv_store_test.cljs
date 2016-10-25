(ns hitch.selectors.kv-store-test
  (:require [cljs.test :refer-macros [is async]]
            [hitch.selector :refer-macros [defselector]]
            [devcards.core :refer-macros [deftest]]
            [hitch.selectors.kv-store :as kv :refer [keyspace keystore-get]]
            [hitch.graph :as graph]
            [hitch.pin :refer [pin]]
            [hitch.mutable.graph :as mgraph]
            [hitch.graphs.graph-manager :as gm]
            [hitch.graphs.immutable :as im]))

(def gctors
  [["Mutable graph: " mgraph/graph]
   ["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1))]])

(doseq [[graph-name gctor] gctors]
  (deftest async-hook-value-change
    (let [graph (gctor)]
      (async done
        (let [ks-sel (keyspace :main)]
          (is (= (get graph (kv/keystore-get ks-sel :test) nil) nil)
            (str graph-name "nil value before keystore is created."))
          (pin graph (kv/keystore-get ks-sel :test))
          (is (= (get graph (kv/keystore-get ks-sel :test) nil) nil)
            (str graph-name "still nil value before keystore has a value."))
          (graph/apply-commands graph [[ks-sel [:set-value {:test :cat}]]])
          (graph/hook graph (fn [v]
                              (is (= v :cat)
                                (str graph-name "Keystore has a value"))
                              (done))
            kv/keystore-get ks-sel :test)))))

  (deftest sync-value-changes
    (let [graph   (gctor)
          testsel (keyspace :main)
          keysel  (keystore-get testsel :test)]
      (is (= (get graph keysel :not-found) :not-found)
        (str graph-name "not-found value before keystore is created."))
      (pin graph keysel)
      (is (= (get graph keysel :not-found) nil)
        (str graph-name "nil value after keystore is created"))
      (graph/apply-commands graph [[testsel [:set-value {:test 7}]]])
      (is (= (get graph keysel) 7)
        (str graph-name "keystore-get value updated syncronously first time"))
      (graph/apply-commands graph [[testsel [:set-value {:test 8}]]])
      (is (= (get graph keysel) 8)
        (str graph-name "keystore-get value updated syncronously second time"))
      (graph/apply-commands graph [[testsel [:set-value {:test 9}]]])
      (is (= (get graph keysel) 9)
        (str graph-name "keystore-get value updated syncronously third time")))))

