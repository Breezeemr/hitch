(ns hitch.selectors.http-test
  (:require-macros [hitch.eager :refer [go]]
                   [cljs.core.async.macros]
                   [hitch.selector :refer [defselector]]
                   [devcards.core :as dc :refer [deftest]])
  (:require [cljs.test :refer [] :refer-macros [is run-tests async]]
            [hitch.core :as core]
            [hitch.protocols :as proto]
            [hitch.selectors.kv-store :as kv :refer [keyspace key]]
            [hitch.graph :as graph]
            [cljs.core.async :as async]
            [hitch.graphs.mutable :as mgraph]))


(deftest firstt
         (let [graph (mgraph/graph)]
           (async done
             (is (= 1 1))
             (done))))

