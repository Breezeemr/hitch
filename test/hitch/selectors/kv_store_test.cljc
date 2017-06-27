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
        (is (= @a-update-count 2) (str graph-name
                                    "Update `a` the correct number of times."))
        (is (= @b-update-count 1) (str graph-name
                                    "Update `b` only once because its path is not touched."))

        (finally
          (unhook-a)
          (unhook-b)))))

  (deftest efficient-get-in-updates
    (let [graph         (gctor)
          ksp           (kv/keyspace :main)

          expected      (volatile! {})
          update-counts (volatile! {})
          unhook-top0   (let [path [:top 0]]
                          (graph/hook-change-sel graph
                            (fn [v]
                              (vswap! update-counts update path (fnil inc 0))
                              (is (= v (get @expected path))
                                (str graph-name
                                  "New value for " (pr-str path) " as expected")))
                            (kv/get-in ksp path)))
          unhook-a11    (let [path [:top 0 :a10 :a11]]
                          (graph/hook-change-sel graph
                            (fn [v]
                              (vswap! update-counts update path (fnil inc 0))
                              (is (= v (get @expected path))
                                (str graph-name
                                  "New value for " (pr-str path) " as expected")))
                            (kv/get-in ksp path)))
          unhook-a12    (let [path [:top 0 :a10 :a12]]
                          (graph/hook-change-sel graph
                            (fn [v]
                              (vswap! update-counts update path (fnil inc 0))
                              (is (= v (get @expected path))
                                (str graph-name
                                  "New value for " (pr-str path) " as expected")))
                            (kv/get-in ksp path)))
          unhook-a13    (let [path [:top 0 :a10 :a13]]
                          (graph/hook-change-sel graph
                            (fn [v]
                              (vswap! update-counts update path (fnil inc 0))
                              (is (= v (get @expected path))
                                (str graph-name
                                  "New value for " (pr-str path) " as expected")))
                            (kv/get-in ksp path)))
          unhook-a20    (let [path [:top 1 :a20]]
                          (graph/hook-change-sel graph
                            (fn [v]
                              (vswap! update-counts update path (fnil inc 0))
                              (is (= v (get @expected path))
                                (str graph-name
                                  "New value for " (pr-str path) "as expected")))
                            (kv/get-in ksp path)))]
      (try
        (testing "Initialize root value"
          (vreset! expected {[:top 0]           {:a10 {:a11 :a11-v
                                                       :a12 :a12-v}}
                             [:top 0 :a10 :a11] :a11-v
                             [:top 0 :a10 :a12] :a12-v
                             [:top 0 :a10 :a13] nil
                             [:top 1 :a20]      {:a21 :a21-v
                                                 :a22 :a22-v}})
          (graph/apply-commands graph
            [[ksp [::kv/reset {:top [{:a10 {:a11 :a11-v
                                            :a12 :a12-v}}
                                     {:a20 {:a21 :a21-v
                                            :a22 :a22-v}}]}]]])



          (let [uc      @update-counts
                uc-top0 (uc [:top 0])
                uc-a11  (uc [:top 0 :a10 :a11])
                uc-a12  (uc [:top 0 :a10 :a12])
                uc-a13  (uc [:top 0 :a10 :a13])
                uc-a20  (uc [:top 1 :a20])]
            (is (= uc-top0 1) (str graph-name "Initial update."))
            (is (= uc-a11 1) (str graph-name "Initial update."))
            (is (= uc-a12 1) (str graph-name "Initial update."))
            (is (= uc-a13 1) (str graph-name "Not-present path should see update with value nil."))
            (is (= uc-a20 1) (str graph-name "Initial update."))))

        (testing "Updates after write"
          (vreset! expected {[:top 0]           {:a10 {:a11 :a11-v
                                                       :a12 :a12-v2
                                                       :a13 :a13-v}}
                             [:top 0 :a10 :a11] :a11-v
                             [:top 0 :a10 :a12] :a12-v2
                             [:top 0 :a10 :a13] :a13-v
                             [:top 1 :a20]      {:a21 :a21-v
                                                 :a22 :a22-v}})
          (graph/apply-commands graph
            [[ksp [::kv/assoc-in [:top 0 :a10] {:a11 :a11-v
                                                :a12 :a12-v2
                                                :a13 :a13-v}]]])
          (let [uc      @update-counts
                uc-top0 (uc [:top 0])
                uc-a11  (uc [:top 0 :a10 :a11])
                uc-a12  (uc [:top 0 :a10 :a12])
                uc-a13  (uc [:top 0 :a10 :a13])
                uc-a20  (uc [:top 1 :a20])]
            (is (= uc-top0 2) (str graph-name "Parent of write point must update."))
            (is (= uc-a11 1) (str graph-name "Child of write point with unchanged value should not update."))
            (is (= uc-a12 2) (str graph-name "Child of write point with changed value should update."))
            (is (= uc-a13 2) (str graph-name "Child of write point which first receives a value should update for the first time."))
            (is (= uc-a20 1) (str graph-name "Sibling of write point should not update."))))

        (testing "Indirect deletion of key"
          (vreset! expected {[:top 0]           {:a10 {:a11 :a11-v
                                                       :a12 :a12-v2}}
                             [:top 0 :a10 :a11] :a11-v
                             [:top 0 :a10 :a12] :a12-v2
                             [:top 0 :a10 :a13] nil
                             [:top 1 :a20]      {:a21 :a21-v
                                                 :a22 :a22-v}})
          (graph/apply-commands graph
            [[ksp [::kv/assoc-in [:top 0 :a10] {:a11 :a11-v
                                                :a12 :a12-v2}]]])

          (let [uc      @update-counts
                uc-top0 (uc [:top 0])
                uc-a11  (uc [:top 0 :a10 :a11])
                uc-a12  (uc [:top 0 :a10 :a12])
                uc-a13  (uc [:top 0 :a10 :a13])
                uc-a20  (uc [:top 1 :a20])]
            (is (= uc-top0 3) (str graph-name "Parent of write point must update."))
            (is (= uc-a11 1) (str graph-name "Child of write point with unchanged value should not update."))
            (is (= uc-a12 2) (str graph-name "Child of write point with changed value should update."))
            (is (= uc-a13 3) (str graph-name "Indirectly deleted child should update."))
            (is (= uc-a20 1) (str graph-name "Sibling of write point should not update."))))

        (finally
          (unhook-top0)
          (unhook-a11)
          (unhook-a12)
          (unhook-a13)
          (unhook-a20))))))
