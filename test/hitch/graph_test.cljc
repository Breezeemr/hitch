(ns hitch.graph-test
  (:require [hitch.selectors.mutable-var :refer [mutable-var]]
            [hitch.graph :as h]
            [hitch.test-common :refer [->Constant ->SelVec ->Variable]]
    #?(:cljs [hitch.mutable.graph :as mgraph])
            [hitch.pin :refer [pin unpin]]
            [hitch.graphs.graph-manager :as gm]
            [hitch.graphs.immutable :as im]
    #?@(:cljs
        [[cljs.test :as t :refer-macros [testing is async]]
         [devcards.core :refer-macros [deftest]]]
        :default
        [
            [clojure.test :refer [deftest testing is]]])))

(def gctors
  [#?(:cljs ["Mutable graph: " mgraph/graph])
   ["Immutable graph: " #(gm/atom-GraphManager (im/->ImmutableGraph 1))]])

(def mvsel (mutable-var :mvsel))

(doseq [[gname gctor] gctors]
  (deftest hook-unshared-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (h/hook-sel g #(vswap! results conj %) (->Constant 0))

      (is (= @results [0])
        (str gname "Unshared hook's cb sees resolved value immediately"))

      (pin g mvsel)

      (is (= @results [0])
        (str gname "Unshared hook's cb not called if selector gains another ext-child"))))


  (deftest hook-unshared-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (h/hook-sel g #(vswap! results conj %) mvsel)

      (is (= @results [])
        (str gname "Unshared hook's cb uncalled before selector resolved."))

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (is (= @results [0])
        (str gname "Unshared hook's cb sees resolved value immediately"))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [0])
        (str gname "Hooks do not get called more than once"))

      (pin g mvsel)

      (is (= @results [0])
        (str gname "Unshared hook's cb not called if selector gains another ext-child"))))

  (deftest hook-shared-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (pin g (->Constant 0))

      (h/hook-sel g #(vswap! results conj %) (->Constant 0))

      (is (= @results [0])
        (str gname "Shared hook's cb sees resolved value immediately"))

      (pin g mvsel)

      (is (= @results [0])
        (str gname "Shared hook's cb not called if selector gains another ext-child"))))


  (deftest hook-shared-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (pin g mvsel)

      (h/hook-sel g #(vswap! results conj %) mvsel)

      (is (= @results [])
        (str gname "Shared hook's cb uncalled before selector resolved."))

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (is (= @results [0])
        (str gname "Shared hook's cb sees resolved value immediately"))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [0])
        (str gname "Hooks do not get called more than once"))

      (unpin g mvsel)

      (is (= @results [0])
        (str gname "Shared hook's cb not called if selector loses another ext-child"))))


  (deftest hook-next-unshared-immediate-resolution
    (let [g          (gctor)
          results    (volatile! [])
          results-mv (volatile! [])
          sv         (->SelVec [mvsel])]
      (pin g mvsel)

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (h/hook-next-sel g #(vswap! results conj %) sv)

      (h/hook-next-sel g #(vswap! results-mv conj %) mvsel)

      (is (= @results [])
        (str gname "Unshared hook-next's cb should not see resolved value before it changes"))

      (is (= @results-mv [])
        (str gname ""))

      (pin g sv)

      (is (= @results [])
        (str gname "Unshared hook-next's cb not called if selector gains another ext-child"))

      (unpin g mvsel)

      (is (= @results [])
        (str gname "Unshared hook-next's cb not called if selector loses another ext-child"))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [[1]])
        (str gname "Unshared hook-next's cb called only after selector's value changes"))

      (is (= @results-mv [1])
        (str gname ""))

      (h/apply-commands g [[mvsel [:set-value 2]]])

      (is (= @results [[1]])
        (str gname "Unshared hook-next's cb is called only once"))))


  (deftest hook-next-unshared-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])
          sv      (->SelVec [mvsel])]
      (pin g mvsel)

      (h/hook-next-sel g #(vswap! results conj %) sv)

      (is (= @results [])
        (str gname "Unshared hook-next's cb should not see unresolved value"))

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (is (= @results [[0]])
        (str gname "Unshared hook-next's cb should see first resolved value if it was unresolved when added"))

      (pin g sv)

      (is (= @results [[0]])
        (str gname "Unshared hook-next's cb not called if selector gains another ext-child"))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [[0]])
        (str gname "Unshared hook-next's cb should be called only once"))))

  (deftest hook-next-shared-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])
          sv      (->SelVec [mvsel])]
      (pin g sv)

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (h/hook-next-sel g #(vswap! results conj %) sv)

      (is (= @results [])
        (str gname "Shared hook-next's cb should not see resolved value before it changes"))

      (unpin g sv)

      (is (= @results [])
        (str gname "Shared hook-next's cb not called if selector loses another ext-child"))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [[1]])
        (str gname "Shared hook-next's cb called only after selector's value changes"))
      ))


  (deftest hook-next-shared-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])]
      (pin g mvsel)

      (h/hook-next-sel g #(vswap! results conj %) mvsel)

      (is (= @results [])
        (str gname "Shared hook-next's cb should not see unresolved value"))

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (is (= @results [0])
        (str gname "Shared hook-next's cb should see first resolved value if it was unresolved when added"))

      (unpin g mvsel)

      (is (= @results [0])
        (str gname "Shared hook-next's cb not called if selector loses another ext-child"))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [0])
        (str gname "Shared hook-next's cb should be called only once"))
      ))


  (deftest hook-change-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])
          unhook  (h/hook-change-sel g #(vswap! results conj %) mvsel)]

      (is (= @results [])
        (str gname "Hook-change's cb should not see initial unresolved value"))

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should see unresolved->resolved value"))

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if the value didn't change"))

      (h/apply-commands g [[mvsel [:clear]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if the value becomes unresolved."))

      (pin g mvsel)

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if selector gains an ext-child"))

      (h/apply-commands g [[mvsel [:set-value 0]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if the value moves from X to unresolved then back to X."))


      (unpin g mvsel)

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if selector loses an ext-child"))


      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [0 1])
        (str gname "Hook-change's cb should be called when value changes."))

      (h/apply-commands g [[mvsel [:set-value 2]]])

      (is (= @results [0 1 2])
        (str gname "Hook-change's cb should be called when value changes again."))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [0 1 2 1])
        (str gname "Hook-change's cb should be called when value changes again, even if to a previously-seen value."))

      (pin g mvsel)

      (unhook)

      (h/apply-commands g [[mvsel [:set-value 3]]])

      (is (= @results [0 1 2 1])
        (str gname "Hook-change's cb should not be called after unhook"))))

  (deftest hook-change-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])
          _       (pin g mvsel)
          _       (h/apply-commands g [[mvsel [:set-value 0]]])
          unhook  (h/hook-change-sel g #(vswap! results conj %) mvsel)]

      (is (= @results [0])
        (str gname "Hook-change's cb should see initial resolved value"))

      (h/apply-commands g [[mvsel [:set-value 1]]])

      (is (= @results [0 1])
        (str gname "Hook-change's cb should see changed value"))

      (unhook)

      (h/apply-commands g [[mvsel [:set-value 2]]])

      (is (= @results [0 1])
        (str gname "Hook-change's cb should not be called after unhook"))))
  )
