(ns hitch.tracking.halt
  #?(:clj
     (:import (hitch.tracking.halt HaltException)
              (clojure.lang IPending IDeref))
     :cljs
     (:require-macros [hitch.tracking.halt :refer [if-clj-target]])))

#?(:clj
   (defmacro if-clj-target [clj-body cljs-body]
     (if (contains? &env '&env)
       `(if (:ns ~'&env) ~cljs-body ~clj-body)
       (if (:ns &env) cljs-body clj-body)))
   :cljs
   (defmacro if-clj-target [clj-body cljs-body] cljs-body))

(defonce ^:private HALT
  (if-clj-target
    (HaltException/getInstance)
    (reify
      IPrintWithWriter
      (-pr-writer [_ writer opts]
        (-write writer "#<HALT>")))))

(defn halt! []
  (if-clj-target
    (HaltException/doThrow)
    (throw HALT)))

(defn #?(:cljs ^boolean halt? :default halt?) [x]
  (if-clj-target
    (HaltException/isHalt x)
    (identical? HALT x)))

(defonce halt-box
  (if-clj-target
    (reify
      IDeref
      (deref [_] (halt!))
      IPending
      (isRealized [_] false))
    (reify
      IDeref
      (-deref [_] (halt!))
      IPending
      (-realized? [_] false))))

(defn select-box [value]
  (if-clj-target
    (reify
      IDeref
      (deref [_] value)
      IPending
      (isRealized [_] true))
    (reify
      IDeref
      (-deref [_] value)
      IPending
      (-realized? [_] true))))

(defmacro maybe-halt
  "Evaluate `expr`; if `expr` halts, evaluate `if-halted-expr` instead."
  [expr if-halted-expr]
  `(try
     ~expr
     (catch ~(if-clj-target HaltException :default) e#
       (if (halt? e#)
         ~if-halted-expr
         (throw e#)))))
