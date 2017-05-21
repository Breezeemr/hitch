(ns hitch.core-test
  (:require [clojure.test :refer [is deftest testing]]
            [hitch.selector :as sel]))

(deftest selectors
  (testing "defselector definition does not error-out"
    (sel/defselector TEST-SELECTOR [G A [B1 B2 :as B] {C1 :k :as C}]
      [A B C]))
  (testing "defselector eval-fn generation"
    (is (= (TEST-SELECTOR-eval-fn nil 1 [2] {:k 3}) [1 [2] {:k 3}])))
  (testing "defselector record property names are correct"
    (let [{:keys [A B C]} (->TEST-SELECTOR-selector 1 [2] {:k 3})]
      (is (= A 1))
      (is (= B [2]))
      (is (= C {:k 3}))))
  (testing "Invoking defselector-ed symbol returns selector record"
    (is (= (->TEST-SELECTOR-selector 1 [2] {:k 3})
          (TEST-SELECTOR 1 [2] {:k 3}))))
  (testing "Defselector-ed symbol implements Selector protocol for inline use."
    (satisfies? hitch.protocol/Selector TEST-SELECTOR)))
