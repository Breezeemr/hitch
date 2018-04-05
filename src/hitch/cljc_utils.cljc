(ns hitch.cljc-utils
  #?(:cljs
     (:require-macros [hitch.cljc-utils :refer [if-clj-target defsatisfies]])
     :clj
     (:import (java.util.concurrent ConcurrentHashMap))))

(defmacro if-clj-target [clj-body cljs-body]
  #?(:clj
     (if (contains? &env '&env)
       `(if (:ns ~'&env) ~cljs-body ~clj-body)
       (if (:ns &env) cljs-body clj-body))
     :cljs
     cljs-body))

;; This is to get around satisfies? being slow in CLJ until
;; https://dev.clojure.org/jira/browse/CLJ-1814 is applied
;; CLJS is already fast
(if-clj-target
  (defn fast-satisfies [protocol-var]
    ;; copied and modified from manifold.utils/fast-satisfies
    ;; Default case is unreachable, but we need to make sure ConcurrentHashMap is
    ;; not seen by the cljs reader
    (let [classes #?(:clj (ConcurrentHashMap. 16 0.9 1) :default nil)]
      (add-watch protocol-var ::memoization
        (fn [_ _ _ _] (.clear classes)))
      (fn [^Object x]
        (let [protocol @protocol-var]
          (if (nil? x)
            (contains? (:impls protocol) nil)
            (if (instance? (:on-interface protocol) x)
              true
              (let [cls (.getClass x)]
                (if-some [s? (.get classes cls)]
                  s?
                  (.putIfAbsent classes cls (satisfies? protocol x))))))))))
  nil)

(defmacro defsatisfies [name protocol-sym]
  (if-clj-target
    `(def ~name (fast-satisfies (var ~protocol-sym)))
    `(defn
       ~(vary-meta name assoc :tag 'boolean)
       ~(vary-meta ['x] assoc :tag 'boolean)
       (satisfies? ~protocol-sym ~'x))))
