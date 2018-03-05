(ns hitch.graphs.mutable-test
  (:require [hitch.protocol :as hp]
            [hitch.selectors.kv-store :as kv]
            [hitch.mutable.graph :as mgraph]
            [hitch.pin :refer [pin unpin]]
            [hitch.graph :as graph]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [cljs.test :as t :refer-macros [testing is async]]
            [devcards.core :refer-macros [deftest]]
            [hitch.selectors.http :as http]))


(deftest simple-get-ok
  (let [graph (mgraph/graph)
        http-sel (http/http "/test.txt" :get nil nil nil nil nil)]
    (async done
      (let [next-phase (fn []
                         ;(pin graph http-sel)
                         (.gc-pass graph)
                         (is (= (get graph http-sel :not-loaded) :not-loaded))

                         (done))]

        (graph/hook graph (fn [[status value :as result]]
                            (is (= result [:ok "cat\n"]))
                            (js/setTimeout next-phase 10)
                            )
          http/http "/test.txt" :get nil nil nil nil nil)))))

(deftest gc-clean-mark-sweep

  ;; Tests gc when the marked nodes are still sweepable at sweep time
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
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) [[5]]))
      (is (= (get graph ff-var ::not-found) [5])))

    (testing "first pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) [[5]]))
      (is (= (get graph ff-var ::not-found) ::not-found)))
    (testing "second pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) ::not-found))
      (is (= (get graph ff-var ::not-found) ::not-found)))
    (testing "third pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) ::not-found))
      (is (= (get graph first-var ::not-found) ::not-found))
      (is (= (get graph ff-var ::not-found) ::not-found)))))

(deftest gc-dirty-mark
  ;; Tests gc when a marked node becomes not-sweepable by time sweep occurs
  (let [graph     (mgraph/graph)
        mvar      (mutable-var :test)
        first-var (kv/get mvar 0)
        ff-var    (kv/get first-var 0)]
    (is (= (get graph (mutable-var :test) ::not-found) ::not-found))
    (pin graph ff-var)
    (is (= (get graph (mutable-var :test) ::not-found) ::not-found))
    (graph/apply-commands graph [[(mutable-var :test) [:set-value [[[5]]]]]])
    (is (= (get graph (mutable-var :test)) [[[5]]]))
    (is (= (get graph first-var) [[5]]))
    (is (= (get graph ff-var) [5]))
    (testing "unpinned"
      (unpin graph ff-var)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) [[5]]))
      (is (= (get graph ff-var ::not-found) [5])))
    (testing "first pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) [[5]]))
      (is (= (get graph ff-var ::not-found) ::not-found)))
    (testing "second pass"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) ::not-found))
      (is (= (get graph ff-var ::not-found) ::not-found)))

    (testing "repin at last moment: "
      (pin graph ff-var)
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test)) [[[5]]]))
      (is (= (get graph first-var) [[5]]))
      (is (= (get graph ff-var) [5])))

    (testing "still pinned"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test)) [[[5]]]))
      (is (= (get graph first-var) [[5]]))
      (is (= (get graph ff-var) [5])))

    (testing "unpinned again"
      (unpin graph ff-var)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) [[5]]))
      (is (= (get graph ff-var ::not-found) [5])))
    (testing "first pass again"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) [[5]]))
      (is (= (get graph ff-var ::not-found) ::not-found)))
    (testing "second pass again"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) [[[5]]]))
      (is (= (get graph first-var ::not-found) ::not-found))
      (is (= (get graph ff-var ::not-found) ::not-found)))

    (testing "fully collected"
      (.gc-pass graph)
      (is (= (get graph (mutable-var :test) ::not-found) ::not-found))
      (is (= (get graph first-var ::not-found) ::not-found))
      (is (= (get graph ff-var ::not-found) ::not-found)))
    ))




