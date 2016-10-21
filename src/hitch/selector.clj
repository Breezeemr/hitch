(ns hitch.selector
    (:require [cljs.core ]
      [clojure.walk :refer [postwalk]]
      [clojure.string :as str]))

(defn eval-selector [eval-fn-name constructor-binding-forms body]
  `(cljs.core/defn ~eval-fn-name ~(into [] (map second) constructor-binding-forms)
     ~@body))

(defn selector-record [selector-name eval-fn-name constructor-binding-forms body]
  (let [graphsymbol (->> constructor-binding-forms ffirst)]
    `(cljs.core/defrecord ~selector-name ~(into [] (map first) (rest constructor-binding-forms))
       hitch.protocol/Selector
       (~'value [~'selector ~graphsymbol ~'state]
         (cljs.core/assert (cljs.core/satisfies? hitch.oldprotocols/IDependencyGraph ~(ffirst constructor-binding-forms)))
         (cljs.core/let [~'dtx (hitch.selector-tx-manager/tx ~graphsymbol ~'selector)]
           ~(->> constructor-binding-forms
                              rest
                              (map first)
                              (cons 'dtx)
                              (cons eval-fn-name)
                              (cons (symbol (str "hitch.selector/attempt")))))))))

(defn sel-constructor [name eval-fn-name selector-name constructor-binding-forms body]
  `(def ~name
     (cljs.core/reify
       hitch.oldprotocols/ISelectorFactory
       (~'inline ~(into ['this] (map first) constructor-binding-forms)
         (cljs.core/assert (cljs.core/satisfies? hitch.oldprotocols/IDependencyGraph ~(ffirst constructor-binding-forms)))
         ~(->> constructor-binding-forms
                            (map first)
                            (cons eval-fn-name)))
       cljs.core/IFn
       (~'-invoke ~(into ['this] (map first) (rest constructor-binding-forms))
         ~(->> constructor-binding-forms
                            (map first)
                            rest
                            (cons (symbol (str "->" selector-name)))))
       )))
(defn create-binding-syms [binding-form]
  (mapv (juxt gensym identity) binding-form))

(defmacro defselector [name constructor-binding-forms & body]
  (let [symbol-binding-pairs (create-binding-syms constructor-binding-forms)
        eval-fn-name (gensym (str name "-eval-fn"))
        selector-name (gensym (str name "-selector"))]
    `(do
       ~(eval-selector eval-fn-name symbol-binding-pairs body)
       ~(selector-record selector-name eval-fn-name symbol-binding-pairs body)
       ~(sel-constructor name eval-fn-name selector-name symbol-binding-pairs body)
       )))
