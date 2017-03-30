(ns hitch.selectors.http-test
  (:require [cljs.test :refer [] :refer-macros [testing is async]]
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
          http/http "/DOES-NOT-EXIST" :get nil nil nil nil nil))))

  (deftest simple-refresh
    (testing "Additional http request is issued after a ::http/refresh command. (Must verify manually in dev console.)"
      (let [graph (gctor)
            sel   (http/http "/test.txt" :get nil nil nil nil nil)]
        (async done
          (graph/hook-sel graph
            (fn [result]
              (is (= result [:ok "cat\n"]) (str graph-name " value before refresh"))

              ;; Second hook is to ensure the selector has not been destroyed
              ;; so the second http request is not elided somehow.
              (graph/hook-sel graph
                (fn [result]
                  (is (= result [:ok "cat\n"]) (str graph-name " value after refresh, next frame"))
                  (done))
                sel)
              (graph/apply-commands graph [[sel [::http/refresh]]])
              (is (= result [:ok "cat\n"]) (str graph-name " value after refresh")))
            sel))))))
