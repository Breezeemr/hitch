(ns hitch.core-test
  (:require [clojure.test :refer [is deftest run-tests]]
            [hitch.selector :as sel]))

(deftest selectors
  (let [binding-pairs '[[G__1 graph] [G__2 a]]]
    (is (= (sel/eval-selector 'evaler binding-pairs ())
           '(cljs.core/defn evaler [graph a] (hitch.eager/go))))
    (is (= (sel/selector-record 'evaler-rec 'evaler-fn binding-pairs ())
           '(cljs.core/defrecord evaler-rec [G__2]
              hitch.protocols/ICreateNode
              (-create-node [this graph] (hitch.nodes.simple/node this))
              cljs.core/IFn
              (-invoke [this G__1]
                (cljs.core/assert (cljs.core/satisfies? hitch.protocols/IDependencyGraph G__1))
                (evaler-fn G__1 G__2)))))
    (is (= (sel/sel-constructor 'evaler 'evaler-fn 'evaler-rec binding-pairs ())
           '(def evaler
              (cljs.core/reify
                hitch.protocols/ISelectorFactory
                (-eval [this G__1 G__2]
                  (cljs.core/assert (cljs.core/satisfies? hitch.protocols/IDependencyGraph G__1))
                  (evaler-fn G__1 G__2))
                (-selector [this G__1 G__2]
                  (cljs.core/assert (cljs.core/satisfies? hitch.protocols/IDependencyGraph G__1))
                  (->evaler-rec G__2))))))))