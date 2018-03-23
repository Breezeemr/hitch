(ns hitch.graphs.immutable-test
  (:require [hitch.graphs.immutable :as im]
            [hitch.graphs.graph-manager :as gm]
            [hitch.protocol :as hp]
            [hitch.test-common :refer [->Constant ->SelVec]]
    #?@(:cljs    [[cljs.test :as t :refer-macros [testing is async]]
                  [devcards.core :refer-macros [deftest]]]
        :default [[clojure.test :refer [deftest testing is]]])))

#?(:clj
   (defmacro async [done-sym & body]
     `(let [done# (atom false)
            ~done-sym (fn [] (reset! done# true))]
        ~@body
        (assert (deref done#) "Async body did not complete!"))))

(defrecord EffectSel [on-command-effect]
  hp/CommandableSelector
  (command-accumulator [s old-state] old-state)
  (command-step [s accumulator command] accumulator)
  (command-result [s accumulator]
    (hp/->StateEffect accumulator on-command-effect nil)))

(deftest basic-graph
  (let [gn           (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
        vec-selector (->SelVec [(->Constant 1) (->Constant 2) (->Constant 3)])
        [status {:keys [value state] :as new-node} {:keys [effect
                                                           selector-changes-by-ext-child
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
    (is (= selector-changes-by-ext-child {:ext1 [vec-selector]}))
    (is (= observable-changed-selector-values {vec-selector [1 2 3]}))))

(deftest garbage-collection
  (let [gn  (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
        c1  (->Constant 1)
        sv4 (->SelVec [c1])
        sv3 (->SelVec [sv4])
        sv2 (->SelVec [sv3])
        sv1 (->SelVec [sv2])
        [status {:keys [value state] :as gn2}]
        (gm/apply-graph-node-commands gn
          [[::hp/child-add sv1 :ext1]])]
    (is (= status :ok))
    (is (= state {c1  (#'im/map->SelectorNode {:value        1
                                               :int-children #{sv4}
                                               :ext-children #{}
                                               :parents      #{}
                                               :state        nil})
                  sv4 (#'im/map->SelectorNode {:value        [1]
                                               :int-children #{sv3}
                                               :ext-children #{}
                                               :parents      #{c1}
                                               :state        nil})
                  sv3 (#'im/map->SelectorNode {:value        [[1]]
                                               :int-children #{sv2}
                                               :ext-children #{}
                                               :parents      #{sv4}
                                               :state        nil})
                  sv2 (#'im/map->SelectorNode {:value        [[[1]]]
                                               :int-children #{sv1}
                                               :ext-children #{}
                                               :parents      #{sv3}
                                               :state        nil})
                  sv1 (#'im/map->SelectorNode {:value        [[[[1]]]]
                                               :int-children #{}
                                               :ext-children #{:ext1}
                                               :parents      #{sv2}
                                               :state        nil})})
      "State is correct")
    (let [[status {:keys [value state] :as gn3}]
          (binding [im/*trace* true]
            (try
              (gm/apply-graph-node-commands gn2
                [[::hp/child-del sv1 :ext1]
                 [::hp/child-add sv4 :ext1]])
              (finally (clojure.pprint/pprint (im/get-trace)))))]
      (is (= status :ok))
      (is (= state {c1  (#'im/map->SelectorNode {:value        1
                                                 :int-children #{sv4}
                                                 :ext-children #{}
                                                 :parents      #{}
                                                 :state        nil})
                    sv4 (#'im/map->SelectorNode {:value        [1]
                                                 :int-children #{sv3}
                                                 :ext-children #{:ext1}
                                                 :parents      #{c1}
                                                 :state        nil})
                    sv3 (#'im/map->SelectorNode {:value        [[1]]
                                                 :int-children #{}
                                                 :ext-children #{}
                                                 :parents      #{sv4}
                                                 :state        nil})}))

      )

    ))



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
