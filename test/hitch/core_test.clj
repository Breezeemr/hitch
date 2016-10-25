(ns hitch.core-test
  (:require [clojure.test :refer [is deftest run-tests]]
            [hitch.selector :as sel]))

(deftest selectors
  (let [binding-pairs '[[G__1 graph] [G__2 a]]]
    (is (= (sel/eval-selector 'evaler binding-pairs ())
           '(cljs.core/defn evaler [graph a])))
    (is (= (sel/selector-record 'evaler-rec 'evaler-fn binding-pairs)
           '(cljs.core/defrecord evaler-rec [G__2]
              hitch.protocol/Selector
              (value [selector G__1 state]
                (cljs.core/let [dtx (hitch.selector-tx-manager/tx G__1 selector)]
                  (hitch.selector/attempt evaler-fn dtx G__2))))))
    (is (= (sel/sel-constructor 'evaler 'evaler-fn 'evaler-rec binding-pairs ())
           '(def evaler
              (cljs.core/reify
                hitch.oldprotocols/ISelectorFactory
                (inline [this G__1 G__2]
                  (cljs.core/assert (cljs.core/satisfies? hitch.oldprotocols/IDependencyGraph G__1))
                  (evaler-fn G__1 G__2))
                cljs.core/IFn
                (-invoke[this G__2]
                  (->evaler-rec G__2))))))))