(ns hitch.selectors.http-test
  (:require-macros [hitch.eager :refer [go]]
                   [hitch.selector :refer [defselector]])
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [devcards.core :as dc :refer-macros [deftest]]
            [hitch.core :as core]
            [hitch.selectors.http :refer [HTTPSelector http]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]
            [hitch.mutable.graph :as mgraph]))


(deftest firstt3
         (let [graph (mgraph/graph)
               node1 (graph/hook graph http "http://0.0.0.0:3449/test.json" :get nil  (.-parse js/JSON) nil nil)]
           (async done
             (go
               (is (= (async/<! node1) "cat"))
               (done)))))

