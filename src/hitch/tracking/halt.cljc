(ns hitch.tracking.halt
  #?(:cljs
     (:require-macros
       [hitch.tracking.halt]
       [hitch.cljc-utils :refer [if-clj-target]])
     :default
     (:require
       [hitch.cljc-utils :refer [if-clj-target]])))

;; NOTE: In order to import these only when the target lang is clj, we need to
;; ensure that import and class forms are not even seen *by the reader* during
;; cljs compilation! This goes for all uses too! Otherwise we will get a
;; class-not-found exception from the analyzer. Not sure if this is a bug.
#?(:clj
   (if-clj-target
     (import
       '(hitch.tracking.halt HaltException)
       '(clojure.lang IPending IDeref))
     nil))

(defonce ^:private HALT
  (if-clj-target
    ;; Default case is unreachable, but we need to make sure HaltException is
    ;; not seen by the reader. See note above the import of HaltException.
    #?(:clj (HaltException/getInstance) :default nil)
    (reify
      IPrintWithWriter
      (-pr-writer [_ writer opts]
        (-write writer "#<HALT>")))))

(defn halt! []
  (if-clj-target
    #?(:clj (HaltException/doThrow) :default nil)
    (throw HALT)))

(defn #?(:cljs ^boolean halt? :default halt?) [x]
  (if-clj-target
    #?(:clj (HaltException/isHalt x) :default nil)
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

(if-clj-target
  (deftype SelectBox [value]
    IDeref
    (deref [_] value)
    IPending
    (isRealized [_] true))
  (deftype SelectBox [value]
    IDeref
    (-deref [_] value)
    IPending
    (-realized? [_] true)))

(defn select-box [value]
  (->SelectBox value))

(defmacro maybe-halt
  "Evaluate `expr`; if `expr` halts, evaluate `if-halted-expr` instead."
  [expr if-halted-expr]
  `(try
     ~expr
     (catch ~(if-clj-target #?(:clj HaltException :default nil) :default) e#
       (if (halt? e#)
         ~if-halted-expr
         (throw e#)))))
