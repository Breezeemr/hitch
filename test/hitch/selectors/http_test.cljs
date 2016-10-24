(ns hitch.selectors.http-test
  (:require [cljs.test :refer [] :refer-macros [is async]]
            [devcards.core :refer-macros [deftest]]
            [hitch.selectors.http :as http]
            [hitch.graph :as graph]
            [hitch.mutable.graph :as mgraph]))


(deftest simple-get
  (let [graph (mgraph/graph)]
    (async done
      (graph/hook graph (fn [v]
                          (is (= v "cat\n"))
                          (done))
        http/http "/test.txt" :get nil nil nil nil))))
