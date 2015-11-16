(ns hitch.core-test
  (:require [cljs.test :refer [] :refer-macros [is deftest run-tests]]
            [hitch.core :as core]
            [hitch.protocols :as proto]
            [hitch.fns :as fns]
            [hitch.kv-store :as kv :refer [keyspace]]
            [hitch.alias :as alias]
            [hitch.graph :as graph])
  (:require-macros [hitch.core :as api]))

(defn score [a]
  #_(api/depend tmap a)
  (api/if-loaded
    [x (graph/hitch-get (fns/apply-sel a))]
    (str x)
    x))

(defn t2 [a]
  (graph/hitch-get (fns/apply-sel score (kv/key a))))

(defn test3 [v]
  (str "cat" (graph/hitch-get (fns/apply-sel t2 v))))

(deftest t1
         (graph/clear! graph/*default-graph*)
         (let [kvs (graph/getn (keyspace))]
           (proto/clear! kvs)
           (is (= (t2 "hi") ""))
           (is (= (test3 "hi") "cat"))
           (proto/set-key kvs "hi" :test5)
           (is (= (t2 "hi") ":test5"))
           (is (= (test3 "hi") "cat:test5"))
           (proto/set-key kvs "hi" :test6)
           (is (= (t2 "hi") ":test6"))
           (is (= (test3 "hi") "cat:test6"))
           ))

(deftest aliases
         (graph/clear! graph/*default-graph*)
         (let [kvs (graph/getn (keyspace))
               sfconfig (alias/alias :storefront/config)]
           ;(proto/clear! kvs)
           ;(proto/clear! alias)
           (is (= (graph/getn sfconfig) :hitch/not-loaded))
           (alias/set-alias :storefront/config (kv/key :test))
           (is (= (graph/getn sfconfig) nil))
           (proto/set-key kvs :test :test5)

           (is (= (graph/getn sfconfig) :test5))
           #_(fns/hitch-apply  score (kv/key :test))

           (alias/set-alias :storefront/config (fns/apply-sel score (kv/key :test)))

           (is (= (graph/getn sfconfig) ":test5"))

         ;(proto/set-key alias "hi3" :test5)
         ;(is (= (t2 "hi3") ":test5"))
         ;(is (= (test3 "hi3") "cat:test5"))
         ;(proto/set-key alias "hi5" :test6)
         ;(is (= (t2 "hi5") ":test6"))
         ;(is (= (test3 "hi5") "cat:test6"))

         ))


#_(defn ^:export run []
  (run-tests))
#_(def hi "hi4")


