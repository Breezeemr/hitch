(ns hitch.selector
    (:refer-clojure :only [])
    (:require [cljs.core :refer :all]
      [clojure.walk :refer [postwalk]]
      [clojure.string :as str]))

(clojure.core/defn eval-selector [eval-fn-name constructor-binding-forms body]
  `(defn ~eval-fn-name ~(clojure.core/into [] (clojure.core/map clojure.core/second) constructor-binding-forms)
     (hitch.eager/go
       ~@body)))

(clojure.core/defn selector-record [selector-name eval-fn-name constructor-binding-forms body]
  (let [graphsymbol (->> constructor-binding-forms clojure.core/ffirst)]
    `(defrecord ~selector-name ~(clojure.core/into [] (clojure.core/map clojure.core/first) (clojure.core/rest constructor-binding-forms))
       hitch.protocol/Selector
       (~'value [~'selector ~graphsymbol ~'state]
         (assert (cljs.core/satisfies? hitch.oldprotocols/IDependencyGraph ~(clojure.core/ffirst constructor-binding-forms)))
         (cljs.core/let [~'dtx (hitch.mutable.tx/tx ~graphsymbol ~'selector)]
           (hitch.selector/handle-selector-value
             ~'dtx
             ~(clojure.core/->> constructor-binding-forms
                                clojure.core/rest
                                (clojure.core/map clojure.core/first)
                                (clojure.core/cons 'dtx)
                                (clojure.core/cons eval-fn-name))))))))

(clojure.core/defn sel-constructor [name eval-fn-name selector-name constructor-binding-forms body]
  `(def ~name
     (cljs.core/reify
       hitch.oldprotocols/ISelectorFactory
       (~'inline ~(clojure.core/into ['this] (clojure.core/map clojure.core/first) constructor-binding-forms)
         (assert (cljs.core/satisfies? hitch.oldprotocols/IDependencyGraph ~(clojure.core/ffirst constructor-binding-forms)))
         ~(clojure.core/->> constructor-binding-forms
                            (clojure.core/map clojure.core/first)
                            (clojure.core/cons eval-fn-name)))
       cljs.core/IFn
       (~'-invoke ~(clojure.core/into ['this] (clojure.core/map clojure.core/first) (clojure.core/rest constructor-binding-forms))
         ~(clojure.core/->> constructor-binding-forms
                            (clojure.core/map clojure.core/first)
                            clojure.core/rest
                            (clojure.core/cons (clojure.core/symbol (clojure.core/str "->" selector-name)))))
       )))
(clojure.core/defn create-binding-syms [binding-form]
  (clojure.core/mapv (clojure.core/juxt clojure.core/gensym clojure.core/identity) binding-form))

(clojure.core/defmacro defselector [name constructor-binding-forms & body]
  (let [symbol-binding-pairs (create-binding-syms constructor-binding-forms)
        eval-fn-name (clojure.core/gensym (clojure.core/str name "-eval-fn"))
        selector-name (clojure.core/gensym (clojure.core/str name "-selector"))]
    `(do
       ~(eval-selector eval-fn-name symbol-binding-pairs body)
       ~(selector-record selector-name eval-fn-name symbol-binding-pairs body)
       ~(sel-constructor name eval-fn-name selector-name symbol-binding-pairs body)
       )))

