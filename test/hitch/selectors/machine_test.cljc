(ns hitch.selectors.machine-test
  (:require [hitch.protocol :as hp]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as h]
            [hitch.test-common :refer [->EchoMachine ->EchoVar
                                       ->LogMachine ->LogVar
                                       ->Constant ->SelVec]]
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

(def gctors
  [["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1)) get-immutable-graph-node]])

(doseq [[graph-name gctor get-node] gctors]
  (deftest cannot-command-machines
    (let [g       (gctor)
          run?    (volatile! false)
          machine (->LogMachine (volatile! []))]
      (h/apply-commands g [[machine
                            [:fn (fn [r g state]
                                   (vreset! run? true)
                                   r)]]])
      (is (false? @run?)
        (str graph-name "Machine should not receive commands"))
      (is (= ::absent (get-node g machine ::absent))
        (str graph-name "Machine should not exist because it received no commands"))))

  (deftest cannot-depend-on-machines-from-the-outside
    (let [g       (gctor)
          machine (->EchoMachine)]
      (pin g machine)
      (is (= ::absent (get-node g machine ::absent))
        (str graph-name "Machine should not be pinnable"))
      (is (= ::absent (get @g machine ::absent))
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

  (deftest machine-can-dep-change
    (let [g         (gctor)
          log       (volatile! [])
          c1        (->Constant 1)
          c2        (->Constant 2)
          c3        (->Constant 3)
          log-var   (->LogVar log)
          machine   (->LogMachine log)]
      (pin g log-var)
      (h/apply-commands g [[log-var
                            [:fn (fn [r g _]
                                   (is (and
                                         (= ::absent (get g c1 ::absent))
                                         (= ::absent (get g c2 ::absent)))
                                     (str graph-name "Machine parent should not yet be visible"))
                                   (assoc r :dep-change {c1 true
                                                         c2 true}))]]])
      (is (= #{c1 c2} (:parents (get-node g machine nil)))
        (str graph-name "Machine should gain requested parents"))

      (h/apply-commands g [[log-var
                            [:fn (fn [r g _]
                                   (is (and
                                         (= 1 (get g c1 ::absent))
                                         (= 2 (get g c2 ::absent)))
                                     (str graph-name "Machine parents from previous dep change should be visible"))
                                   (assoc r :dep-change {c1 false
                                                         c2 false
                                                         c3 true}))]]])

      (is (= #{c3} (:parents (get-node g machine nil)))
        (str graph-name "Machine should be able to add and remove deps in same command"))

      (h/apply-commands g [[log-var
                            [:fn (fn [r g _]
                                   (is (and
                                         (= 3 (get g c3 ::absent)))
                                     (str graph-name "Machine parents from previous dep change should be visible"))
                                   (assoc r :dep-change {c1 false
                                                         c2 false
                                                         c3 false}))]]])
      (is (empty? (:parents (get-node g machine nil)))
        (str graph-name "Machine should be able to remove all deps"))
      ))

  (deftest machine-can-var-reset-on-child-add
    (let [g         (gctor)
          log       (volatile! [])
          log-var   (->LogVar log)
          other-var (->EchoVar 0)
          seen-vals (volatile! [])]
      (pin g log-var)
      (pin g other-var)

      (h/apply-commands g
        [[log-var [:fn (fn [r g _]
                         (vswap! seen-vals conj (get g other-var ::absent))
                         (assoc r :dep-change {other-var true}))]]])
      (prn @log)
      (vreset! log [])
      (h/apply-commands g [[other-var [:reset! 1]]])

      (is (= @log [[:commands 3 [[::hp/parent-value-change other-var]]]])
        (str graph-name "Machine should be notified of value change on parent var owned by another machine"))
      (is (= [0] @seen-vals)
        (str graph-name "Machine should see current var value only once"))

      ))

  (deftest machine-sees-parent-update-from-other-var
    (let [g         (gctor)
          log       (volatile! [])
          log-var   (->LogVar log)
          other-var (->EchoVar 0)
          seen-vals  (volatile! [])
          log-var-cmd [:fn (fn [r g _]
                             (vswap! seen-vals conj (get g other-var ::absent))
                             (assoc r :dep-change {other-var true}))]]
      (pin g log-var)
      (pin g other-var)

      (vreset! log [])

      (h/apply-commands g
        [[log-var log-var-cmd]])

      (is (= [[:commands 1 [::hp/var-command log-var log-var-cmd]]]
            @log)
        "Machine should only see dep-change command, never parent-value-change from pre-existing other-var.")

      (is (= [0] @seen-vals)
        (str graph-name "Machine should see current other-var value"))

      (vreset! log [])
      (h/apply-commands g [[other-var [:reset! 1]]])


      (is (= @log [[:commands 2 [[::hp/parent-value-change other-var]]]])
        (str graph-name "Machine should be notified of value change on parent var owned by another machine"))


      )
    )

  ;; Not sure if this test should pass
  ;; Protocol mentions deps applied before var-resets, but I don't remember why?
  #_(deftest var-reset-propagates-immediately
      (let [g         (gctor)
            log       (volatile! [])
            log-var   (->LogVar log)
            other-var (->EchoVar 0)
            seen-val  (volatile! nil)]
        (pin g log-var)
        (pin g other-var)

        (h/apply-commands g
          [[other-var [:reset! 1]]
           [log-var [:fn (fn [r g _]
                           (if (nil? @seen-val)
                             (vreset! seen-val (get g other-var))
                             (vreset! seen-val ::seen-twice))
                           (assoc r :dep-change {other-var true}))]]])
        (is (= 1 @seen-val)
          (str graph-name "Var reset should be visible on the graph during command application"))))

  (deftest machine-sees-parent-update-from-selector)

  (deftest machine-does-not-see-own-var-update)

  (deftest machine-does-not-see-parent-update-for-realized-unchanging-parent)

  (deftest only-vars-can-dep-machines)

  (deftest cannot-var-reset-non-vars)

  (deftest can-var-reset-only-own-vars)

  (deftest selectors-see-var-resets)

  (deftest cannot-external-dep-a-machine
    (let [g       (gctor)
          machine (->EchoMachine)]
      (pin g machine)
      (is (= ::absent (get-node g machine ::absent))
        "Should not be possible to depend on a machine externally")))

  )
