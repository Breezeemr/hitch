(ns hitch.selectors.machine-test
  (:import #?(:clj (clojure.lang ExceptionInfo)))
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

(defn- select-all-keys [m ks nf]
  (into {}
    (map #(do [% (get m % nf)]))
    ks))

(defn- get-immutable-graph-node [g sel nf]
  ;; Don't type hint GraphValues: something tricky with redefs will cause
  ;; "java.lang.ClassCastException: GraphValues cannot be cast to GraphValues"
  (-> @g (.-selnodes) (get sel nf)))

(def gctors
  [["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1)) get-immutable-graph-node]])

(doseq [[graph-name gctor get-node] gctors]
  (deftest cannot-command-machines
    (let [g       (gctor)
          log     (volatile! [])
          machine (->LogMachine log)]
      (h/apply-commands g [[machine [:fn (fn [r _g _sn] r)]]])
      (is (empty? @log)
        (str graph-name "Machine should not receive commands"))))

  (deftest cannot-depend-on-machines-from-the-outside
    (let [g       (gctor)
          machine (->EchoMachine)]
      (pin g machine)
      (is (= ::absent (get-node g machine ::absent))
        (str graph-name "Machine should not be pinnable"))))

  (deftest selectors-cannot-depend-on-machines
    (let [g       (gctor)
          machine (->EchoMachine)
          sel     (->SelVec [machine])
          pin-sel (try
                    (pin g sel)
                    (catch ExceptionInfo e
                      ::threw))]

      (is (= ::threw pin-sel)
        (str graph-name "Ordinary selectors cannot depend on machines"))))

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
        (str graph-name
          "Machine should be able to add and remove deps in same command"))

      (h/apply-commands g [[log-var
                            [:fn (fn [r g _]
                                   (is (and
                                         (= 3 (get g c3 ::absent)))
                                     (str graph-name "Machine parents from previous dep change should be visible"))
                                   (assoc r :dep-change {c1 false
                                                         c2 false
                                                         c3 false}))]]])
      (is (empty? (:parents (get-node g machine nil)))
        (str graph-name "Machine should be able to remove all deps"))))

  (deftest machine-can-var-reset-on-child-add
    (let [g        (gctor)
          echo-var (->EchoVar 0)]
      (pin g echo-var)
      (is (= 0 (get @g echo-var))
        (str graph-name
          "Machine can var-reset its own Var as soon as it is ::hp/child-add-ed"))))

  (deftest machine-does-not-see-parent-update-for-realized-unchanging-parent
    (let [g            (gctor)
          log          (volatile! [])
          log-var      (->LogVar log)
          realized-sel (->Constant 0)
          log-cmd      [:log (fn [_r g _sn]
                               (select-all-keys g [realized-sel] ::absent))]
          dep-cmd      [:fn (fn [r _g _sn]
                              (assoc r :dep-change {realized-sel true}))]]
      (pin g realized-sel)
      (pin g log-var)

      (vreset! log [])

      (h/apply-commands g [[log-var log-cmd] [log-var dep-cmd]])
      (let [[cmds seen-val & other] @log]
        (is (= cmds [:commands 1 [[::hp/var-command log-var log-cmd]
                                  [::hp/var-command log-var dep-cmd]]]))

        (is (= seen-val [:log 1 {realized-sel 0}])
          (str graph-name
            "Machine should see value of realized selector before adding as a parent"))

        (is (empty? other)
          (str graph-name
            "Machine should not be notified of parent-value-change after adding as parent if parent value did not change")))))

  (deftest machine-sees-parent-update-from-created-parent
    (let [g          (gctor)
          log        (volatile! [])
          log-var    (->LogVar log)
          parent-sel (->Constant 0)
          log-cmd    [:log (fn [_r g _sn]
                             (select-all-keys g [parent-sel] ::absent))]
          dep-cmd    [:fn (fn [r _g _sn]
                            (assoc r :dep-change {parent-sel true}))]]
      (pin g log-var)

      (vreset! log [])

      (h/apply-commands g [[log-var log-cmd] [log-var dep-cmd]])
      (let [[cmds seen-val parent-val-chg parent-val & other] @log]
        (is (= cmds [:commands 1 [[::hp/var-command log-var log-cmd]
                                  [::hp/var-command log-var dep-cmd]]]))

        (is (= seen-val [:log 1 {parent-sel ::absent}])
          (str graph-name
            "Machine should not see value of unrealized selector before adding as a parent"))

        (is (= parent-val-chg [:commands 2 [[::hp/parent-value-change parent-sel]]])
          (str graph-name "Machine should see parent-value-change of newly-created parent"))

        (is (= parent-val [:new-parent-value 2 parent-sel 0])
          (str graph-name "Machine should see new parent val at moment of parent-value-change"))

        (is (empty? other)
          (str graph-name "Machine should not see any other updates")))))

  (deftest machine-sees-parent-update-from-other-var
    (let [g           (gctor)
          log         (volatile! [])
          log-var     (->LogVar log)
          other-var   (->EchoVar 0)
          log-var-cmd [:fn (fn [r _g _sn]
                             (assoc r :dep-change {other-var true}))]]
      (pin g log-var)
      (pin g other-var)

      (vreset! log [])

      (h/apply-commands g [[log-var log-var-cmd]])

      (vreset! log [])
      (h/apply-commands g [[other-var [:reset! 1]]])

      (let [[parent-change-command new-parent-value & other] @log]
        (is (= parent-change-command
              [:commands 2 [[::hp/parent-value-change other-var]]])
          (str graph-name
            "Machine should be notified of value change on parent var owned by another machine"))

        (is (= new-parent-value [:new-parent-value 2 other-var 1])
          (str graph-name
            "Machine should see new parent value when notified of its change"))

        (is (empty? other)
          (str graph-name "No other commands should be issued")))))

  (deftest machine-sees-parent-update-from-selector
    (let [g           (gctor)
          log         (volatile! [])
          log-var     (->LogVar log)
          distant-sel (mutable-var :foo)
          parent-sel  (->SelVec [distant-sel])
          log-cmd     [:log (fn [_r g _sn]
                              (select-all-keys g [parent-sel] ::absent))]
          dep-cmd     [:fn (fn [r _g _sn]
                             (assoc r :dep-change {parent-sel true}))]]
      (pin g log-var)
      (pin g distant-sel)

      (vreset! log [])

      (h/apply-commands g [[log-var log-cmd]
                           [log-var dep-cmd]])

      (is (= [[:commands 1 [[::hp/var-command log-var log-cmd]
                            [::hp/var-command log-var dep-cmd]]]
              [:log 1 {parent-sel ::absent}]] @log)
        (str graph-name
          "Machine parent value should not be visible"))

      (vreset! log [])

      (h/apply-commands g [[distant-sel [:set-value 1]]])

      (let [[parent-change-command new-parent-value & other] @log]
        (is (= parent-change-command
              [:commands 2 [[::hp/parent-value-change parent-sel]]])
          (str graph-name
            "Machine should be notified of value change on parent selector"))

        (is (= new-parent-value [:new-parent-value 2 parent-sel [1]])
          (str graph-name
            "Machine should see new parent value when notified of its change"))

        (is (empty? other)
          (str graph-name "No other commands should be issued")))))

  (deftest var-reset-without-children-is-not-saved
    (let [g       (gctor)
          log     (volatile! [])
          log-var (->LogVar log)
          rst-cmd [:fn (fn [r _g _sn]
                         (assoc r :var-reset {log-var 1}))]]

      (h/apply-commands g [[log-var rst-cmd]])

      (is (= ::absent (get-node g log-var ::absent))
        (str graph-name "Var-reset on unused vars should not be visible in graph"))))

  (deftest machine-does-not-see-own-var-update
    (let [g         (gctor)
          log       (volatile! [])
          log-var   (->LogVar log)
          log-cmd   [:log (fn [_r g _sn]
                            (select-all-keys g [log-var] ::absent))]
          dep-cmd   [:fn (fn [r _g _sn]
                           (assoc r
                             :var-reset {log-var 1}
                             :dep-change {log-var true}))]
          reset-cmd [:fn (fn [r _g _sn]
                           (assoc r :var-reset {log-var 2}))]]

      (pin g log-var)

      (h/apply-commands g [[log-var dep-cmd]])

      (h/apply-commands g [[log-var log-cmd]
                           [log-var reset-cmd]])

      (is (->> @log
               (filter (fn [[type]] (= type :commands)))
               (mapcat (fn [[_ _ cmds]]
                         (map first cmds)))

               (not-any? #(= ::hp/parent-value-change %)))
        (str graph-name "Machine should never receive parent-value-change for its own vars"))))

  (deftest selectors-dont-see-unrealized-vars
    (let [g         (gctor)
          log       (volatile! [])
          log-var   (->LogVar log)
          sel       (->SelVec [log-var])
          reset-cmd [:fn (fn [r _g _sn]
                           (assoc r :var-reset {log-var 1}))]]

      (pin g sel)

      (is (= ::absent (get @g sel ::absent))
        (str graph-name "Selector with unset var parent should not be realized"))

      (h/apply-commands g [[log-var reset-cmd]])

      (is (= [1] (get @g sel ::absent))
        (str graph-name "Selector should see var-reset of unrealized var"))))

  (deftest selectors-see-var-resets
    (let [g         (gctor)
          var       (->EchoVar 0)
          sel       (->SelVec [var])]

      (pin g sel)

      (is (= [0] (get @g sel ::absent))
        (str graph-name
          "Selector should see parent var-reset issued in same tx as selector creation"))

      (h/apply-commands g [[var [:reset! 1]]])

      (is (= [1] (get @g sel ::absent))
        (str graph-name
          "Realized selectors should see parent var-resets"))))

  (deftest cannot-var-reset-non-vars
    (let [g         (gctor)
          log       (volatile! [])
          log-var   (->LogVar log)
          mut-var   (mutable-var :foo)
          reset-cmd [:fn (fn [r _g _sn]
                           (assoc r :var-reset {mut-var 1}))]]

      (pin g mut-var)

      (is (= ::threw (try
                       (h/apply-commands g [[log-var reset-cmd]])
                       (catch ExceptionInfo e ::threw)))
        (str graph-name "Machines cannot var-reset non-vars"))))

  (deftest can-var-reset-only-own-vars
    (let [g         (gctor)
          log       (volatile! [])
          log-var   (->LogVar log)
          reset-cmd [:fn (fn [r _g _sn]
                           (assoc r :var-reset {(->EchoVar 0) 1}))]]

      (pin g log-var)

      (is (= ::threw (try
                       (h/apply-commands g [[log-var reset-cmd]])
                       (catch ExceptionInfo e ::threw)))
        (str graph-name "Machines cannot var-reset other machine's vars"))))

  (deftest machine-effects-run
    (let [g       (gctor)
          run     (volatile! [])
          log-var (->LogVar (volatile! []))]
      (h/apply-commands g [[log-var
                            [:fn (fn [r _g _sn]
                                   (assoc r
                                     :effect (fn [_] (vswap! run conj true))))]]])
      (is (= [true] @run)
        (str graph-name "Machine effects should run"))))

  )
