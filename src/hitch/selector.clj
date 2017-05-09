(ns hitch.selector
    (:require
      [cljs.core]
      [clojure.walk :refer [postwalk]]
      [clojure.string :as str]))

(defn cljs-target? [env]
  (some? (:ns env)))

(defn eval-selector [eval-fn-name constructor-binding-forms body]
  `(defn ~eval-fn-name ~(into [] (map second) constructor-binding-forms)
     ~@body))

(defn selector-record [selector-name eval-fn-name constructor-binding-forms]
  (let [graphsymbol (->> constructor-binding-forms ffirst)]
    `(defrecord ~selector-name ~(into [] (map first) (rest constructor-binding-forms))
       hitch.protocol/Selector
       (~'value [~'selector ~graphsymbol ~'state]
         (let [~'dtx (hitch.selector-tx-manager/tx ~graphsymbol ~'selector)]
           (hitch.selector/attempt ~eval-fn-name ~'dtx
             ~@(map first (rest constructor-binding-forms))))))))

(defn sel-constructor
  [env name eval-fn-name selector-name constructor-binding-forms body]
  `(def ~name
     (reify
       hitch.oldprotocols/ISelectorFactory
       (~'inline ~(into ['this] (map first) constructor-binding-forms)
         (assert (satisfies? hitch.oldprotocols/IDependencyGraph ~(ffirst constructor-binding-forms)))
         ~(->> constructor-binding-forms
                            (map first)
                            (cons eval-fn-name)))
       ~(if (cljs-target? env)
          'cljs.core/IFn
          'clojure.lang.IFn)
       (~(if (cljs-target? env)
           '-invoke
           'invoke)
         ~(into ['this] (map first) (rest constructor-binding-forms))
         ~(->> constructor-binding-forms
                            (map first)
                            rest
                            (cons (symbol (str "->" selector-name))))))))

(defn create-binding-syms [binding-form]
  (mapv (juxt gensym identity) binding-form))

(defmacro defselector [name constructor-binding-forms & body]
  (let [symbol-binding-pairs (create-binding-syms constructor-binding-forms)
        eval-fn-name (gensym (str name "-eval-fn"))
        selector-name (symbol (str name "-selector"))]
    `(do
       ~(eval-selector eval-fn-name symbol-binding-pairs body)
       ~(selector-record selector-name eval-fn-name symbol-binding-pairs)
       ~(sel-constructor &env name eval-fn-name selector-name symbol-binding-pairs body))))
