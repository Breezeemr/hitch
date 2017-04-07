(ns hitch.selectors.mutable-var-test
  (:require [cljs.test :refer [] :refer-macros [is async testing]]
            [devcards.core :as dc :refer-macros [deftest]]
            [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as graph]
            [hitch.mutable.graph :as mgraph]
            [hitch.pin :refer [pin unpin]]
            [hitch.graphs.graph-manager :as gm]
            [hitch.graphs.immutable :as im]))

(def gctors
  [["Mutable graph: " mgraph/graph]
   ["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1))]])

(doseq [[graph-name gctor] gctors]

  (deftest node-creation
    (let [graph (gctor)]
      (is (= (get graph (mutable-var :test) ::not-found) ::not-found)
        (str graph-name "New graph should have nil value for selector."))
      (pin graph (mutable-var :test))
      (is (= (get graph (mutable-var :test) ::not-found) ::not-found)
        (str graph-name "After pinning, before state change, selector should still be not-found."))
      (graph/apply-commands graph [[(mutable-var :test) [:set-value 5]]])
      (is (= (get graph (mutable-var :test)) 5)
        (str graph-name "Graph should have value for selector after selector state change."))))

  (deftest nested-hook-next
    (testing "Nested `hook-next`s see value changes to a selector in the order they occur."
      (async done
        (let [graph   (gctor)
              testsel (mutable-var :test)
              firstfn (fn [val]
                        (is (= val 7) (str graph-name "First hook"))
                        (graph/hook-next graph
                          (fn [val]
                            (is (= val 8) (str graph-name "Second hook (nested)"))
                            (graph/hook-next graph
                              (fn [val]
                                (is (= val 9) (str graph-name "Third hook (nested)"))
                                (done))
                              mutable-var :test)
                            (graph/apply-commands graph [[testsel [:set-value 9]]]))
                          mutable-var :test)
                        (graph/apply-commands graph [[testsel [:set-value 8]]]))]
          (graph/hook graph firstfn mutable-var :test)
          (graph/apply-commands graph [[testsel [:set-value 7]]])))))

  (deftest hitch-callback
    (testing "Hitch-callback is only triggered when all its dependencies are satisfied."
      (async done
        (let [graph    (gctor)
              testsel  (mutable-var :test)
              testsel2 (mutable-var :test2)
              callback (fn [result]
                         (is (= (:a result) 7) (str graph-name ":a result"))
                         (is (= (:b result) 8) (str graph-name ":b result"))
                         (done))
              firstfn  (fn [tx]
                         {:a @(graph/select-sel! tx testsel)
                          :b @(graph/select-sel! tx testsel2)})]
          (graph/hitch-callback graph callback firstfn)
          (graph/apply-commands graph [[testsel [:set-value 7]]])
          (graph/apply-commands graph [[testsel2 [:set-value 8]]])))))

  (deftest hooks-triggered-once-syncronously
    (testing "When multiple hooks are attached, they will all get triggered
  only with the first value change and only see the first value.
  (For immutable graphs, this invariant is preserved by the tx-watcher's
  scheduling of change notifications.)"
      (async done
        (let [graph (gctor)]
          (let [testsel (mutable-var :test)]
            (graph/hook graph
              (fn [val]
                (is (= val 7) (str graph-name "First simultaneous hook.")))
              mutable-var :test)
            (graph/hook graph
              (fn [val]
                (is (= val 7) (str graph-name "Second simultaneous hook.")))
              mutable-var :test)
            (graph/hook graph
              (fn [val]
                (is (= val 7) (str graph-name "Third simultaneous hook.")))
              mutable-var :test)
            (graph/apply-commands graph [[testsel [:set-value 7]]])
            (graph/apply-commands graph [[testsel [:set-value 8]]])
            (graph/apply-commands graph [[testsel [:set-value 9]]])))
        (done)))))
