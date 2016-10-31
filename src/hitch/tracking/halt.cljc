(ns hitch.tracking.halt
  (:require [hitch.protocol.selector :as psel]
            [hitch.selector.common :refer [resolution-value]]
            [hitch.protocol.tracking :as ptrack]
            [hitch.tracking.common :as track]
            [hitch.util :refer [UNKNOWN unknown?]])
  #?(:clj
     (:import (hitch.tracking HaltException)
              (clojure.lang IPending IDeref))))


(defonce ^:private HALT
  #?(:cljs
     (reify
       IPrintWithWriter
       (-pr-writer [_ writer opts]
         (-write writer "#<HALT>")))
     :clj
     (doto (HaltException.)
       (.setStackTrace (make-array StackTraceElement 0)))))

(defn halt! [] (throw HALT))

(defn halt? [x] (identical? HALT x))

(defn dget-haltable [unknown haltable tracker args]
  (try
    (apply haltable tracker args)
    (catch #?(:cljs :default :clj HaltException) e
      (if (halt? e)
        unknown
        (throw e)))))

(defn resolve-haltable [haltable lookup args]
  (let [tracker (track/simple-tracker lookup)
        v       (dget-haltable UNKNOWN haltable tracker args)
        parents (ptrack/-get-depends tracker)]
    (if (unknown? v)
      (psel/->Unresolved parents)
      (psel/->Resolved v parents))))

(defn invoke-haltable [haltable lookup args]
  (let [tracker (track/simple-tracker lookup)
        v       (dget-haltable UNKNOWN haltable tracker args)
        parents (ptrack/-get-depends tracker)]
    (if (unknown? v)
      (psel/->Unresolved parents)
      (psel/->Resolved v parents))))


(defonce halt-box
  #?(:cljs (reify
             IDeref
             (-deref [_] (tc/halt!))
             IPending
             (-realized? [_] false))
     :clj  (reify
             IDeref
             (deref [_] (tc/halt!))
             IPending
             (isRealized [_] false))))

(defn select-box [value]
  #?(:cljs (reify
             IDeref
             (-deref [_] value)
             IPending
             (-realized? [_] true))
     :clj  (reify
             IDeref
             (deref [_] value)
             IPending
             (isRealized [_] true))))
