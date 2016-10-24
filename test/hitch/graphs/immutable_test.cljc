(ns hitch.graphs.immutable-test
  (:require [hitch.graphs.immutable :as im]
            [hitch.graphs.graph-manager :as gm]
            [hitch.protocol :as hp]
    #?@(:cljs    [[cljs.test :as t :refer-macros [testing is]]
                  [devcards.core :refer-macros [deftest]]]
        :default [[clojure.test :refer [deftest testing is]]])))

(defrecord Constant [v]
  hp/Selector
  (value [_ _ _]
    (hp/->SelectorValue v nil)))

(defrecord Variable [name]
  hp/Selector
  (value [s sv state] state)
  hp/CommandableSelector
  (command-accumulator [s old-state] old-state)
  (command-step [s accumulator command]
    (case (first command)
      :set! (second command)
      (hp/map->CommandError {:accumulator accumulator
                             :error       "Unrecognized Command"})))
  (command-result [s accumulator]
    (hp/->StateEffect accumulator nil nil)))

(defrecord SelVec [parents]
  hp/Selector
  (value [_ sv _]
    (let [v (reduce
              (fn [r parent]
                (let [x (get sv parent ::not-found)]
                  (if (= x ::not-found)
                    (reduced ::not-found)
                    (conj r x))))
              [] parents)]
      (if (= v ::not-found)
        (hp/->SelectorUnresolved parents)
        (hp/->SelectorValue v parents)))))


(def const-1 (->Constant 1))

(def myvar (->Variable "myvar"))

(deftest prepare-ops
  (is (= (#'im/prepare-ops {} {}) {}) "Nothing to do")
  (testing "Elidable ops"
    (is (= (#'im/prepare-ops {}
             {const-1 {::im/update-ext-children {:del #{:a}}}})
          {})
      "Degenerate case of removing ext children from non-existent node.")
    (is (= (#'im/prepare-ops {}
             {myvar {::im/commands [[:set! 42]]}})
          {})
      "Degenerate case of commanding non-existent node with no deps."))
  (testing "Existing node"
    (is (= (#'im/prepare-ops
             {const-1 {}}
             {const-1 {::im/update-ext-children {:add #{:a}}}})
          {const-1 {::im/update-ext-children {:add #{:a}}}}))
    (is (= (#'im/prepare-ops
             {const-1 {}}
             {const-1 {::im/update-ext-children {:del #{:a}}}})
          {const-1 {::im/update-ext-children {:del #{:a}}}}))
    (is (= (#'im/prepare-ops
             {myvar {}}
             {myvar {::im/commands [[:set! 42]]}})
          {myvar {::im/commands [[:set! 42]]}})))
  (is (= (#'im/prepare-ops
           {myvar {}}
           {myvar          {::im/commands [[:set! 42]]}
            const-1        {::im/update-ext-children {:add #{:a}}}
            (->Constant 2) {::im/update-ext-children {:del #{:b}}}})
        {myvar   {::im/commands [[:set! 42]]}
         const-1 {::im/update-ext-children {:add #{:a}}}})
    "Mixed ops"))

(defn create-imgraph-with-sel [selector]
  (let [gn (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
        [status {:keys [value state]} _]
        (gm/apply-graph-node-commands gn
          [[::hp/child-add selector :ext1]])]
    {:status status
     :value  value
     :state  state}))

(deftest basic-graph
  (let [gn           (:graph-node (gm/create-graph-node (im/->ImmutableGraph 1)))
        vec-selector (->SelVec [(->Constant 1) (->Constant 2) (->Constant 3)])
        [status {:keys [value state] :as new-node} {:keys [effect recalc-external-children]}]
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
    (is (= (set recalc-external-children) #{:ext1}))))
