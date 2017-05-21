(ns hitch.selector
  #?(:cljs (:require-macros hitch.selector))
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.tracking.halt :as halt]
            [hitch.selector-tx-manager]))

(defn- cljs-target? [env]
  (some? (:ns env)))

(defn- sel-constructor
  [env name eval-fn-name selector-name param-names]
  `(def ~name
     (reify
       hitch.oldprotocols/ISelectorFactory
       (~'inline [_# dtx# ~@param-names]
         (assert (satisfies? hitch.oldprotocols/IDependencyGraph dtx#))
         (~eval-fn-name dtx# ~@param-names))

       ~(if (cljs-target? env)
          'cljs.core/IFn
          'clojure.lang.IFn)
       (~(if (cljs-target? env)
           '-invoke
           'invoke)
         [_# ~@param-names]
         (~(symbol (str "->" selector-name)) ~@param-names)))))

(defn- param-names [binding-form]
  (mapv (fn [x]
          (assert (not= x '&)
            "Variadic parameters are not allowed on defselector.")
          (cond
            (symbol? x) x
            (and (map? x) (:as x)) (:as x)
            (and (vector? x)
              (>= (count x) 2)
              (= (-> x pop peek) :as)) (peek x)
            :else
            (throw (ex-info "Every parameter to defselector must have a name, either directly or with a top-level :as"
                     {:binding-form binding-form
                      :bad-param x}))))
    binding-form))

(defmacro defselector [name constructor-binding-forms & body]
  (let [record-field-names   (param-names (rest constructor-binding-forms))
        eval-fn-name         (symbol (str name "-eval-fn"))
        selector-name        (symbol (str name "-selector"))]
    `(do
       (defn ~eval-fn-name ~constructor-binding-forms ~@body)
       (defrecord ~selector-name ~record-field-names
         hitch.protocol/Selector
         (~'value [selector# graph# state#]
           (let [dtx# (hitch.selector-tx-manager/tx graph# selector#)]
             (hitch.selector/attempt ~eval-fn-name dtx# ~@record-field-names))))
       ~(sel-constructor &env name eval-fn-name selector-name record-field-names))))

(defn create-resolved-value
  ([vfn tx-manager]
   (proto/->SelectorValue (vfn tx-manager)
     (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a]
   (proto/->SelectorValue (vfn tx-manager a)
     (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b]
   (proto/->SelectorValue (vfn tx-manager a b)
     (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c]
   (proto/->SelectorValue (vfn tx-manager a b c)
     (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d]
   (proto/->SelectorValue (vfn tx-manager a b c d)
     (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d e]
   (proto/->SelectorValue (vfn tx-manager a b c d e)
     (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d e f]
   (proto/->SelectorValue (vfn tx-manager a b c d e f)
     (oldproto/get-depends tx-manager)))
  ([vfn tx-manager a b c d e f g]
   (proto/->SelectorValue (vfn tx-manager a b c d e f g)
     (oldproto/get-depends tx-manager))))

(defn create-unresolved-value [tx-manager]
  (proto/->SelectorUnresolved (oldproto/get-depends tx-manager)))

(defn attempt
  ([vfn tx-manager]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager)
     (create-unresolved-value tx-manager)))
  ([vfn tx-manager a]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager a)
     (create-unresolved-value tx-manager)))
  ([vfn tx-manager a b]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager a b)
     (create-unresolved-value tx-manager)))
  ([vfn tx-manager a b c]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager a b c)
     (create-unresolved-value tx-manager)))
  ([vfn tx-manager a b c d]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager a b c d)
     (create-unresolved-value tx-manager)))
  ([vfn tx-manager a b c d e]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager a b c d e)
     (create-unresolved-value tx-manager)))
  ([vfn tx-manager a b c d e f]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager a b c d e f)
     (create-unresolved-value tx-manager)))
  ([vfn tx-manager a b c d e f g]
   (halt/maybe-halt
     (create-resolved-value vfn tx-manager a b c d e f g)
     (create-unresolved-value tx-manager))))
