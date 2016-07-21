(ns hitch.core-test
  (:require [clojure.test :refer [is deftest run-tests]]
            [hitch.selector :as sel]))

(deftest selectors
  (let [binding-pairs '[[G__2 a]]]
    (is (= (sel/eval-selector 'evaler binding-pairs ())
           '(cljs.core/defn evaler [graph a] (hitch.eager-go/eager-go))))
    (is (= (sel/selector-record 'evaler-rec 'evaler-fn binding-pairs ())
           '(cljs.core/defrecord evaler-rec [G__2]
              hitch.protocols/ICreateNode
              (-create-node [this graph] (hitch.nodes.simple/node this))
              cljs.core/IFn
              (-invoke [this graph]
                (cljs.core/assert graph)
                (evaler-fn graph G__2)))))
    (is (= (sel/sel-constructor 'evaler 'evaler-fn 'evaler-rec binding-pairs ())
           '(def evaler
              (cljs.core/reify
                hitch.protocols/ISelector
                (-eval [this graph G__2]
                  (cljs.core/assert graph)
                  (evaler-fn graph G__2))
                (-selector [this graph G__2]
                  (cljs.core/assert graph)
                  (->evaler-rec G__2))))))))