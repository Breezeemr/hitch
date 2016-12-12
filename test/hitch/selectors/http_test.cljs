(ns hitch.selectors.http-test
  (:require [cljs.test :refer [] :refer-macros [is async]]
            [devcards.core :refer-macros [deftest]]
            [hitch.selectors.http :as http]
            [hitch.graph :as graph]
            [hitch.mutable.graph :as mgraph]
            [hitch.graphs.graph-manager :as gm]
            [hitch.graphs.immutable :as im]))

(def gctors
  [["Mutable graph" mgraph/graph]
   ["Immutable graph" #(gm/atom-GraphManager (im/->ImmutableGraph 1))]])

(doseq [[graph-name gctor] gctors]
  (deftest simple-get-ok
    (let [graph (gctor)]
      (async done
        (graph/hook graph (fn [[status value :as result]]
                            (is (= result [:ok "cat\n"]) graph-name)
                            (done))
          http/http "/test.txt" :get nil nil nil nil nil))))

  (deftest simple-get-error
    (let [graph (gctor)]
      (async done
        (graph/hook graph (fn [[status error :as result]]
                            (is (= status :error) graph-name)
                            (done))
          http/http "/DOES-NOT-EXIST" :get nil nil nil nil nil)))))
