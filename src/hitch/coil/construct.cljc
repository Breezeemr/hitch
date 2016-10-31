(ns hitch.coil.construct
  (:require [hitch.coil.common :refer [coil-kind]]
            [hitch.tracking.halt :as halt]
            [hitch.tracking.common :as track]
            [hitch.protocol.selector :as psel]
            [hitch.util :refer [rsome throw-ex-info]]))

(defn mark-selector-provider! [f]
  (alter-meta! f assoc :hitch.coil/fn-kind :hitch.coil.kind/selector-provider))

(defn mark-resolver! [f]
  (alter-meta! f assoc :hitch.coil/fn-kind :hitch.coil.kind/resolver))

(defn mark-haltable! [f]
  (alter-meta! f assoc :hitch.coil/fn-kind :hitch.coil.kind/haltable))

(defn- rget [kw coll] (kw coll))

(defn haltable->resolver [haltable]
  (mark-resolver!
    (fn [tracker & args]
      (halt/resolve-haltable tracker haltable args))))

(defn haltable->resolver [haltable]
  (mark-resolver!
    (fn [tracker & args]
      (halt/resolve-haltable tracker haltable args))))

(defn selector->resolver* [selector]
  (mark-resolver!
    (fn [tracker]
      (psel/-resolution selector tracker nil))))

(defn selector->resolver [selector]
  (if (psel/stateless-selector? selector)
    (selector->resolver* selector)
    (throw-ex-info
      "Cannot coerce selector which uses state, commands or effects to a resolver"
      {:selector selector})))

#?(:clj
   (defmacro defselector
     "Define a Selector record with the given body and return a braid with:

     :hitch.coil.braid/name
     :hitch.coil.kind/selector-provider
     :hitch.coil.kind/resolver
     :hitch.coil.kind/haltable

     The body should be written as a haltable, i.e. it must have a tracker
     as the first argument and it may call select or dget, and must either
     return a value or halt (i.e. throw a halt exception)."
     [selector-name paramvec & body]
     (assert (and (symbol? selector-name) (nil? (namespace selector-name))))
     (assert (vector? paramvec))
     (assert (pos? (count paramvec)))
     (let [haltable-fn-name (gensym (str selector-name "_haltable_"))
           haltable-fn      `(defn ~haltable-fn-name
                               {:hitch.coil/fn-kind :hitch.coil.kind/haltable}
                               ~paramvec ~@body)
           resolver-fn-name (gensym (str selector-name "_resolver_"))
           resolver-fn      `(defn ~resolver-fn-name
                               {:hitch.coil/fn-kind :hitch.coil.kind/resolver}
                               ~paramvec
                               (halt/resolve-haltable ~haltable-fn-name
                                 ~(first paramvec) ~(vec (rest paramvec))))
           fqname           ~(symbol (ns-name *ns*) (name selector-name))
           record-ctor-name ~(symbol (namespace fqname) (str "->"
                                                          (name fqname)))]
       `(do
          ~haltable-fn
          ~resolver-fn
          (defrecord ~selector-name ~(vec (rest paramvec))
            psel/Selector
            (~'-resolution [~'_ graph-vals# ~'_]
              (halt/resolve-haltable ~haltable-fn-name graph-vals#
                ~(vec (rest paramvec)))))
          (alter-meta! ~record-ctor-name assoc :hitch.coil/fn-kind
            :hitch.coil.kind/selector-provider)
          {:hitch.coil.braid/name             ~(str fqname)
           :hitch.coil.kind/selector-provider ~record-ctor-name
           :hitch.coil.kind/resolver          ~resolver-fn-name
           :hitch.coil.kind/haltable          ~haltable-fn-name}))))
