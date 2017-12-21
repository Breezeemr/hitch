(ns hitch.graphs.mutable-test
  (:require [hitch.protocol :as hp]
            [hitch.selectors.kv-store :as kv]
            [hitch.mutable.graph :as mgraph]
            [hitch.pin :refer [pin unpin]]
            [hitch.graph :as graph]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [cljs.test :as t :refer-macros [testing is async]]
            [devcards.core :refer-macros [deftest]]))


(deftest gc
  (let [graph (mgraph/graph)
        mvar  (mutable-var :test)
        first-var (kv/get mvar 0)
        ff-var (kv/get first-var 0)]
    (is (= (get graph (mutable-var :test) ::not-found) ::not-found))
    (pin graph ff-var)
    (is (= (get graph (mutable-var :test) ::not-found) ::not-found))
    (graph/apply-commands graph [[(mutable-var :test) [:set-value [[[5]]]]]])
    (is (= (get graph (mutable-var :test)) [[[5]]]))
    (is (= (get graph first-var) [[5]]))
    (is (= (get graph ff-var) [5]))
    (testing "unpinned"
      (unpin graph ff-var)
      (is (= (get graph (mutable-var :test) :hitch.graphs.mutable-test/not-found) [[[5]]]))
      (is (= (get graph first-var :hitch.graphs.mutable-test/not-found) [[5]]))
      (is (= (get graph ff-var :hitch.graphs.mutable-test/not-found) [5])))

    (testing "first pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) :hitch.graphs.mutable-test/not-found) [[[5]]]))
      (is (= (get graph first-var :hitch.graphs.mutable-test/not-found) [[5]]))
      (is (= (get graph ff-var :hitch.graphs.mutable-test/not-found) :hitch.graphs.mutable-test/not-found)))
    (testing "second pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) :hitch.graphs.mutable-test/not-found) [[[5]]]))
      (is (= (get graph first-var :hitch.graphs.mutable-test/not-found) :hitch.graphs.mutable-test/not-found))
      (is (= (get graph ff-var :hitch.graphs.mutable-test/not-found) :hitch.graphs.mutable-test/not-found)))
    (testing "third pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) :hitch.graphs.mutable-test/not-found) :hitch.graphs.mutable-test/not-found))
      (is (= (get graph first-var :hitch.graphs.mutable-test/not-found) :hitch.graphs.mutable-test/not-found))
      (is (= (get graph ff-var :hitch.graphs.mutable-test/not-found) :hitch.graphs.mutable-test/not-found)))))

