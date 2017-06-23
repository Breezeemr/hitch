(ns hitch.selectors.kv-store-test
  (:require [hitch.selectors.kv-store :as kv]
            [hitch.graph :as graph]
            [hitch.pin :refer [pin]]
    #?(:cljs [hitch.mutable.graph :as mgraph])
            [hitch.graphs.graph-manager :as gm]
            [hitch.graphs.immutable :as im]
    #?@(:cljs
        [[cljs.test :as t :refer-macros [testing is async]]
         [devcards.core :refer-macros [deftest]]]
        :default
        [[clojure.test :refer [deftest testing is]]])))

#?(:clj
   (defmacro async [done-sym & body]
     `(let [done# (atom false)
            ~done-sym (fn [] (reset! done# true))]
        ~@body
        (assert (deref done#) "Async body did not complete!"))))

(def gctors
  [#?(:cljs ["Mutable graph: " mgraph/graph])
   ["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1))]])

(doseq [[graph-name gctor] gctors]
  (deftest async-hook-value-change
    (let [graph (gctor)]
      (async done
        (let [ks-sel (kv/keyspace :main)]
          (is (= (get graph (kv/get ks-sel :test) ::nf) ::nf)
            (str graph-name "not-found value before keystore is created."))
          (pin graph (kv/get ks-sel :test))
          (is (= (get graph (kv/get ks-sel :test) ::nf) ::nf)
            (str graph-name "not-found value on missing keys."))
          (graph/apply-commands graph [[ks-sel [::kv/reset {:test :cat}]]])
          (graph/hook graph (fn [v]
                              (is (= v :cat)
                                (str graph-name "Keystore has a value"))
                              (done))
            kv/get ks-sel :test)))))

  (deftest sync-value-changes
    (let [graph   (gctor)
          testsel (kv/keyspace :main)
          keysel  (kv/get testsel :test)]
      (is (= (get graph keysel ::nf) ::nf)
        (str graph-name "not-found value before keystore is created."))
      (pin graph keysel)
      (is (= (get graph keysel ::nf) ::nf)
        (str graph-name "not-found value after keystore is created"))
      (graph/apply-commands graph [[testsel [::kv/reset {:test 7}]]])
      (is (= (get graph keysel) 7)
        (str graph-name "keystore-get value updated synchronously first time"))
      (graph/apply-commands graph [[testsel [::kv/reset {:test 8}]]])
      (is (= (get graph keysel) 8)
        (str graph-name "keystore-get value updated synchronously second time"))
      (graph/apply-commands graph [[testsel [::kv/reset {:test 9}]]])
      (is (= (get graph keysel) 9)
        (str graph-name "keystore-get value updated synchronously third time"))))

  (deftest efficient-get-updates
    (let [graph          (gctor)
          testsel        (kv/keyspace :main)

          expected-a     (volatile! nil)
          a-update-count (volatile! 0)
          b-update-count (volatile! 0)

          unhook-a       (graph/hook-change-sel graph
                           (fn [a]
                             (vswap! a-update-count inc)
                             (is (= a @expected-a)))
                           (kv/get testsel :a))
          unhook-b       (graph/hook-change-sel graph
                           (fn [b]
                             (vswap! b-update-count inc)
                             (is (= b :b-value)))
                           (kv/get testsel :b))]
      (try
        (vreset! expected-a {:a2 :a-value-1})
        (graph/apply-commands graph [[testsel [::kv/reset {:a {:a2 :a-value-1}
                                                           :b :b-value}]]])

        (vreset! expected-a {:a2 :a-value-2})
        (graph/apply-commands graph [[testsel [::kv/assoc-in [:a :a2] :a-value-2]]])
        (is (= @a-update-count 2) "Update `a` the correct number of times.")
        (is (= @b-update-count 1) "Update `b` only once because its path is not touched.")

        (finally
          (unhook-a)
          (unhook-b)))))

  #_(deftest efficient-get-in-updates
      ;;TODO
      ))
