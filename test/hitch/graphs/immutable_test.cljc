(ns hitch.graphs.immutable-test
  (:require [hitch.graphs.immutable :as im]
            [hitch.graphs.graph-manager :as gm]
            [hitch.protocol :as hp]
            [hitch.selectors.mutable-var :as mv]
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
        [status {:keys [value state] :as gn2} {:keys [effect
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
    (is (= observable-changed-selector-values {vec-selector [1 2 3]}))

    (testing "observable-changed-selector-values should have all selectors mentioned by selector-changes-by-ext-child"
      (let [unresolved-sel (mv/mutable-var :unresolved)
            [status _ {:keys [selector-changes-by-ext-child
                              observable-changed-selector-values]}]
            (gm/apply-graph-node-commands gn2
              [[::hp/child-add vec-selector :ext2]
               [::hp/child-add unresolved-sel :ext2]])]
        (is (= status :ok))
        (is (= selector-changes-by-ext-child {:ext2 [vec-selector]})
          "newly-added ext-children get recalced even if selector value does not change")
        (is (= observable-changed-selector-values {vec-selector [1 2 3]})
          "observable-changed-selector-values should include values that did not change if the selector has an ext-child that needs recalc"))

      (let [unresolved-sel (mv/mutable-var :unresolved)
            [status _ {:keys [selector-changes-by-ext-child
                              observable-changed-selector-values]}]
            (gm/apply-graph-node-commands gn2
              [[::hp/child-add unresolved-sel :ext2]])]
        (is (= status :ok))
        (is (empty? selector-changes-by-ext-child)
          "newly-added ext-children do not get recalced selector value is unresolved")
        (is (empty? observable-changed-selector-values)
          "observable-changed-selector-values should not include unresolved values"))
      )))

(deftest garbage-collection
  (let [gn  (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
        c1  (->Constant 1)
        sv1 (->SelVec [c1])
        sv2 (->SelVec [sv1])
        sv3 (->SelVec [sv2])
        sv4 (->SelVec [sv3])
        [status {:keys [value state] :as gn2}]
        (gm/apply-graph-node-commands gn
          [[::hp/child-add sv4 :ext1]])]
    (is (= status :ok))
    (is (= state {c1  (#'im/map->SelectorNode {:value        1
                                               :int-children #{sv1}
                                               :ext-children #{}
                                               :parents      #{}
                                               :state        nil})
                  sv1 (#'im/map->SelectorNode {:value        [1]
                                               :int-children #{sv2}
                                               :ext-children #{}
                                               :parents      #{c1}
                                               :state        nil})
                  sv2 (#'im/map->SelectorNode {:value        [[1]]
                                               :int-children #{sv3}
                                               :ext-children #{}
                                               :parents      #{sv1}
                                               :state        nil})
                  sv3 (#'im/map->SelectorNode {:value        [[[1]]]
                                               :int-children #{sv4}
                                               :ext-children #{}
                                               :parents      #{sv2}
                                               :state        nil})
                  sv4 (#'im/map->SelectorNode {:value        [[[[1]]]]
                                               :int-children #{}
                                               :ext-children #{:ext1}
                                               :parents      #{sv3}
                                               :state        nil})})
      "State is correct")
    (let [[status {:keys [value state] :as gn3}]
          (gm/apply-graph-node-commands gn2
            [[::im/do-gc 0]
             [::hp/child-del sv4 :ext1]
             [::hp/child-add sv1 :ext1]])]
      (is (= status :ok))
      (is (= state {c1  (#'im/map->SelectorNode {:value        1
                                                 :int-children #{sv1}
                                                 :ext-children #{}
                                                 :parents      #{}
                                                 :state        nil})
                    sv1 (#'im/map->SelectorNode {:value        [1]
                                                 :int-children #{sv2}
                                                 :ext-children #{:ext1}
                                                 :parents      #{c1}
                                                 :state        nil})
                    sv2 (#'im/map->SelectorNode {:value        [[1]]
                                                 :int-children #{sv3}
                                                 :ext-children #{}
                                                 :parents      #{sv1}
                                                 :state        nil})
                    sv3 (#'im/map->SelectorNode {:value        [[[1]]]
                                                 :int-children #{sv4}
                                                 :ext-children #{}
                                                 :parents      #{sv2}
                                                 :state        nil})
                    sv4 (#'im/map->SelectorNode {:value        [[[[1]]]]
                                                 :int-children #{}
                                                 :ext-children #{}
                                                 :parents      #{sv3}
                                                 :state        nil})})
        "GC off should never delete nodes"))

    (let [[status {:keys [value state] :as gn3}]
          (gm/apply-graph-node-commands gn2
            [[::hp/child-del sv4 :ext1]
             [::hp/child-add sv1 :ext1]])]
      (is (= status :ok))
      (is (= state {c1  (#'im/map->SelectorNode {:value        1
                                                 :int-children #{sv1}
                                                 :ext-children #{}
                                                 :parents      #{}
                                                 :state        nil})
                    sv1 (#'im/map->SelectorNode {:value        [1]
                                                 :int-children #{sv2}
                                                 :ext-children #{:ext1}
                                                 :parents      #{c1}
                                                 :state        nil})
                    sv2 (#'im/map->SelectorNode {:value        [[1]]
                                                 :int-children #{sv3}
                                                 :ext-children #{}
                                                 :parents      #{sv1}
                                                 :state        nil})
                    sv3 (#'im/map->SelectorNode {:value        [[[1]]]
                                                 :int-children #{}
                                                 :ext-children #{}
                                                 :parents      #{sv2}
                                                 :state        nil})})
        "Default GC should collect only one level and leave no dangling :int-children references."))

    (let [[status {:keys [value state] :as gn3}]
          (binding [im/*trace* true]
            (gm/apply-graph-node-commands gn2
              [[::im/do-gc :full]
               [::hp/child-del sv4 :ext1]
               [::hp/child-add sv1 :ext1]]))]
      (is (= status :ok))
      (is (= state {c1  (#'im/map->SelectorNode {:value        1
                                                 :int-children #{sv1}
                                                 :ext-children #{}
                                                 :parents      #{}
                                                 :state        nil})
                    sv1 (#'im/map->SelectorNode {:value        [1]
                                                 :int-children #{}
                                                 :ext-children #{:ext1}
                                                 :parents      #{c1}
                                                 :state        nil})})
        "Full GC should collect all nodes without children"))))

(deftest children-del-all-command
  (let [gn  (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
        c1  (->Constant 1)
        sv1 (->SelVec [c1])
        sv2 (->SelVec [sv1])
        sv3 (->SelVec [sv2])
        sv4 (->SelVec [sv3])
        [status {:keys [state] :as gn2}]
        (gm/apply-graph-node-commands gn
          [[::hp/child-add sv4 :ext1]
           [::hp/child-add sv4 :ext2]
           [::hp/child-add sv4 :ext3]
           [::hp/child-add sv4 :ext4]
           [::hp/child-add sv3 :ext1]
           [::hp/child-add sv3 :ext2]
           [::hp/child-add sv3 :ext3]
           [::hp/child-add sv3 :ext4]
           [::hp/child-add sv2 :ext1]
           [::hp/child-add sv2 :ext2]
           [::hp/child-add sv2 :ext3]
           [::hp/child-add sv2 :ext4]
           [::hp/child-add sv1 :ext1]
           [::hp/child-add sv1 :ext2]
           [::hp/child-add sv1 :ext3]
           [::hp/child-add sv1 :ext4]])]
    (is (= status :ok))
    (is (= (select-keys state [sv1 sv2 sv3 sv4])
          {sv1 (#'im/map->SelectorNode {:value        [1]
                                        :int-children #{sv2}
                                        :ext-children #{:ext1 :ext2 :ext3 :ext4}
                                        :parents      #{c1}
                                        :state        nil})
           sv2 (#'im/map->SelectorNode {:value        [[1]]
                                        :int-children #{sv3}
                                        :ext-children #{:ext1 :ext2 :ext3 :ext4}
                                        :parents      #{sv1}
                                        :state        nil})
           sv3 (#'im/map->SelectorNode {:value        [[[1]]]
                                        :int-children #{sv4}
                                        :ext-children #{:ext1 :ext2 :ext3 :ext4}
                                        :parents      #{sv2}
                                        :state        nil})
           sv4 (#'im/map->SelectorNode {:value        [[[[1]]]]
                                        :int-children #{}
                                        :ext-children #{:ext1 :ext2 :ext3 :ext4}
                                        :parents      #{sv3}
                                        :state        nil})})
      "State is correct")

    (let [[status {:keys [state]}]
          (gm/apply-graph-node-commands gn2
            [[::im/children-del-all [:ext1 :ext2 :ext3]]])]
      (is (= status :ok))
      (is (= (select-keys state [sv1 sv2 sv3 sv4])
            {sv1 (#'im/map->SelectorNode {:value        [1]
                                          :int-children #{sv2}
                                          :ext-children #{:ext4}
                                          :parents      #{c1}
                                          :state        nil})
             sv2 (#'im/map->SelectorNode {:value        [[1]]
                                          :int-children #{sv3}
                                          :ext-children #{:ext4}
                                          :parents      #{sv1}
                                          :state        nil})
             sv3 (#'im/map->SelectorNode {:value        [[[1]]]
                                          :int-children #{sv4}
                                          :ext-children #{:ext4}
                                          :parents      #{sv2}
                                          :state        nil})
             sv4 (#'im/map->SelectorNode {:value        [[[[1]]]]
                                          :int-children #{}
                                          :ext-children #{:ext4}
                                          :parents      #{sv3}
                                          :state        nil})})
        "::im/children-del-all should remove all matching :ext-children from graph"))))

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


(deftest atom-graph-manager-supports-ExternalDependent2
  (let [gm     (gm/atom-GraphManager (im/->ImmutableGraph 1))
        result (volatile! nil)
        ext    (reify
                 hp/ExternalDependent2
                 (-change-notify2 [_ g parents]
                   (vreset! result
                     (into {}
                       (map #(do [% (get g % ::not-found)]))
                       parents))))
        _      (hp/transact! gm [[::hp/child-add (->Constant 1) ext]])]
    (is (= @result {(->Constant 1) 1})
      "After update, -change-notify2 is called with the correct arguments.")))
