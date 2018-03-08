(ns hitch.graphs.immutable-test
  (:require [hitch.graphs.immutable :as im]
            [hitch.graphs.graph-manager :as gm]
            [hitch.protocol :as hp]
            [hitch.test-common :refer :all]
    #?@(:cljs    [[cljs.test :as t :refer-macros [testing is async]]
                  [devcards.core :refer-macros [deftest]]]
        :default [[clojure.test :refer [deftest testing is]]])))

(defrecord EffectSel [on-command-effect]
  hp/CommandableSelector
  (command-accumulator [s old-state] old-state)
  (command-step [s accumulator command] accumulator)
  (command-result [s accumulator]
    (hp/->StateEffect accumulator on-command-effect nil)))

(deftest basic-graph
  (let [gn           (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
        vec-selector (->SelVec [(->Constant 1) (->Constant 2) (->Constant 3)])
        [status {:keys [value state] :as new-node} {:keys [effect recalc-external-children
                                                           observable-changed-selector-values]}]
        (gm/apply-graph-node-commands gn
          [[::hp/child-add vec-selector :ext1]])]
    (is (= status :ok))
    (is (= state
          {vec-selector   (#'im/map->SelectorNode {:value        [1 2 3]
                                                   :int-children #{}
                                                   :ext-children #{:ext1}
                                                   :parents      #{(->Constant 1)
                                                                   (->Constant 2)
                                                                   (->Constant 3)}
                                                   :state        nil})
           (->Constant 1) (#'im/map->SelectorNode {:value        1
                                                   :int-children #{vec-selector}
                                                   :ext-children #{}
                                                   :parents      #{}
                                                   :state        nil})
           (->Constant 2) (#'im/map->SelectorNode {:value        2
                                                   :int-children #{vec-selector}
                                                   :ext-children #{}
                                                   :parents      #{}
                                                   :state        nil})
           (->Constant 3) (#'im/map->SelectorNode {:value        3
                                                   :int-children #{vec-selector}
                                                   :ext-children #{}
                                                   :parents      #{}
                                                   :state        nil})
           })
      "State is correct")
    (testing "Value ILookup state-wrapper Works"
      (is (= (get value (->Constant 1)) 1))
      (is (= (get value (->Constant 2)) 2))
      (is (= (get value (->Constant 3)) 3))
      (is (= (get value vec-selector) [1 2 3])))
    (is (nil? effect))
    (is (= (set recalc-external-children) #{:ext1}))
    (is (= observable-changed-selector-values {vec-selector [1 2 3]}))))

(deftest always-run-effects
  (testing "Graph should always run effects, even when a selector node exists only for the lifetime of the transaction."
    (async done
      (let [gn (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
            did-run (volatile! false)
            effect-fn (fn [] (vreset! did-run true) (done))
            [status {:keys [value state] :as new-node} {:keys [effect recalc-external-children]}]
            (gm/apply-graph-node-commands gn
              [[::hp/command (->EffectSel effect-fn) [:do-something]]])]
        (is (= status :ok))
        (is (= state {}) "EffectSel should be gone because not depended on.")
        (is (fn? effect) "We should get an effect, even though the selector that created it is gone.")
        (when effect (effect))
        (is @did-run "Effect should run, even though selector was created and destroyed within a single TX.")))))
