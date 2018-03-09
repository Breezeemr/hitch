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
        [[clojure.test :refer [deftest testing is]]])))


(defn- get-immutable-graph-node [g sel nf]
  ;; Don't type hint GraphValues: something tricky with redefs will cause
  ;; "java.lang.ClassCastException: GraphValues cannot be cast to GraphValues"
  (-> @g (.-selnodes) (get sel nf)))

(def fn-machine (->FnMachine))

(def gctors
  [["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1)) get-immutable-graph-node]])

(doseq [[graph-name gctor get-node] gctors]
  (deftest cannot-command-machines
    (let [g    (gctor)
          run? (volatile! false)]
      (h/apply-commands g [[fn-machine
                            [:do (fn [r g state]
                                   (vreset! run? true)
                                   r)]]])
      (is (false? @run?) (str graph-name "Machine should not receive commands"))
      (is (= ::absent (get-node g fn-machine ::absent))
        "Machine should not exist because it received no commands")))

  (deftest cannot-depend-on-machines-from-the-outside
    (let [g (gctor)]
      (pin g fn-machine)
      (is (= ::absent (->> ::absent (get-node g fn-machine)))
        "Machine should not be pinnable")
      (is (= ::absent (get @g fn-machine ::absent))
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
      (is (= ::absent (get-node g fn-machine ::absent))
        "Should not be possible to depend on a machine externally")))

  )
