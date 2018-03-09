(ns hitch.selectors.machine-test
  (:require [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as h]
            [hitch.test-common :refer [->FnMachine ->FnVar ->EchoMachine
                                       ->EchoVar]]
            [hitch.pin :refer [pin unpin]]
            [hitch.graphs.graph-manager :as gm]
            [hitch.graphs.immutable :as im]
    #?@(:cljs
        [[devcards.core :refer-macros [deftest]]
         [cljs.test :as t :refer-macros [testing is async]]]
        :default
        [[clojure.test :refer [deftest testing is]]]))
  #?(:clj
     (:import (hitch.graphs.immutable GraphValues))))


(defn- get-immutable-graph-node [g sel nf]
  (-> ^GraphValues @g (.-selnodes) (get sel nf)))

(def fn-machine (->FnMachine))

(def gctors
  [["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1)) get-immutable-graph-node]])

(doseq [[graph-name gctor get-node] gctors]
  (deftest create-and-orphan-machine
    (let [g    (gctor)
          run? (volatile! false)]
      (h/apply-commands g [[fn-machine
                            [:do (fn [r g state]
                                   (assoc r :effect (fn [_]
                                                      (vreset! run? true))))]]])
      (is @run? (str graph-name "Effect should run"))
      (is (= ::notfound (get-node g fn-machine ::notfound))
        "Machine should not exist because no-one requires it")))

  (deftest create-and-self-pin-machine
    (let [g (gctor)]
      (h/apply-commands g [[fn-machine
                            [:do (fn [r g state]
                                   (assoc r :dep-change {fn-machine true}))]]])
      (is (get-node g fn-machine nil)
        "Machine should exist because it depends on itself")
      (is (= ::notfound (get @g fn-machine ::notfound))
        "Machine should never have a value")))

  (deftest create-machine-via-var)

  (deftest orphan-var)

  (deftest machine-sees-parent-update-from-other-var)

  (deftest machine-sees-parent-update-from-selector)

  (deftest machine-does-not-see-own-var-update)

  (deftest machine-does-not-see-parent-update-for-realized-unchanging-parent)

  (deftest only-vars-can-dep-machines)

  (deftest cannot-var-reset-non-vars)

  (deftest can-var-reset-only-own-vars)

  (deftest cannot-external-dep-a-machine
    (let [g (gctor)]
      (pin g fn-machine)
      (is (= ::notfound (get-node g fn-machine ::notfound))
        "Should not be possible to depend on a machine externally")))

  )
