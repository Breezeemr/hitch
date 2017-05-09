(ns hitch.core-test
  (:require [clojure.test :refer [is deftest run-tests]]
            [hitch.selector :as sel]))

(deftest selectors
  (let [binding-pairs '[[G__1 graph] [G__2 a]]]
    (is (= (sel/eval-selector 'evaler binding-pairs ())
           '(clojure.core/defn evaler [graph a])))
    (is (= (sel/selector-record 'evaler-rec 'evaler-fn binding-pairs)
           '(clojure.core/defrecord evaler-rec [G__2]
              hitch.protocol/Selector
              (value [selector G__1 state]
                (clojure.core/let [dtx (hitch.selector-tx-manager/tx G__1 selector)]
                  (hitch.selector/attempt evaler-fn dtx G__2))))))
    (is (= (sel/sel-constructor nil 'evaler 'evaler-fn 'evaler-rec binding-pairs ())
           '(def evaler
              (clojure.core/reify
                hitch.oldprotocols/ISelectorFactory
                (inline [this G__1 G__2]
                  (clojure.core/assert (clojure.core/satisfies? hitch.oldprotocols/IDependencyGraph G__1))
                  (evaler-fn G__1 G__2))
                clojure.lang.IFn
                (invoke[this G__2]
                  (->evaler-rec G__2))))))))
