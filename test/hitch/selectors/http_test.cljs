(ns hitch.selectors.http-test
  (:require-macros [hitch.eager :refer [go]]
                   [cljs.core.async.macros]
                   [hitch.selector :refer [defselector]])
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [devcards.core :as dc :refer-macros [deftest]]
            [hitch.core :as core]
            [hitch.protocols :as proto]
            [hitch.selectors.http :refer []]
            [hitch.graph :as graph]
            [cljs.core.async :as async2]
            [hitch.graphs.mutable :as mgraph]))


(deftest firstt3
         (let [graph (mgraph/graph)]
           (async done
             (is (= 1 1))
             (done))))

