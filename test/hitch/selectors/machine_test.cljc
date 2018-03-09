(ns hitch.selectors.machine-test
  (:require [hitch.protocol :as hp]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as h]
            [hitch.test-common :refer [->FnMachine ->FnVar ->EchoMachine
                                       ->EchoVar ->LogMachine ->LogVar]]
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
      (is (false? @run?)
        (str graph-name "Machine should not receive commands"))
      (is (= ::absent (get-node g fn-machine ::absent))
        (str graph-name "Machine should not exist because it received no commands"))))

  (deftest cannot-depend-on-machines-from-the-outside
    (let [g (gctor)]
      (pin g fn-machine)
      (is (= ::absent (get-node g fn-machine ::absent))
        (str graph-name "Machine should not be pinnable"))
      (is (= ::absent (get @g fn-machine ::absent))
        (str graph-name "Machine should never have a value"))))

  (deftest create-machine-via-var
    (let [g (gctor)
          log (volatile! [])
          log-var (->LogVar log)
          log-machine (->LogMachine log)]
      (testing "Var creation"
        (pin g log-var)
        (is (get-node g log-var nil)
          (str graph-name "Var should exist because pinned"))
        (is (get-node g log-machine nil)
          (str graph-name "Machine should exist because its var is pinned"))
        (let [[a b & xs] @log]
          (is (= a [:create])
            (str graph-name "Machine's create method should be called"))
          (is (= b [:commands 0 [[::hp/child-add log-var]]])
            (str graph-name "Machine should be notified of added var dependency"))
          (is (empty? xs)
            (str graph-name "No other events should happen to the Machine"))))
      (testing "Var removal"
        (vreset! log [])
        (unpin g log-var)
        (is (nil? (get-node g log-var nil))
          (str graph-name "Var should not exist because unpinned"))
        (is (nil? (get-node g log-var nil))
          (str graph-name "Machine should not exist because unpinned"))
        (let [[a b & xs] @log]
          (is (= a [:commands 1 [[::hp/child-del log-var]]])
            (str graph-name "Machine should be notified of removed var dependency"))
          (is (= b [:destroy 2])
            (str graph-name "Machine should be destroyed with latest machine state"))
          (is (empty? xs)
            (str graph-name "No other events should happen to the machine"))))))

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
