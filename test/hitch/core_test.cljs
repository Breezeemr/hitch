(ns hitch.core-test
  (:require-macros [hitch.selector :refer [defselector]])
  (:require [cljs.test :refer [] :refer-macros [is deftest run-tests async]]
            [hitch.core :as core]
            [hitch.selectors.kv-store :as kv :refer [keyspace]]
            [hitch.graph :as graph]))


;
;(defn -score [graph node a]
;  (api/if-loaded
;    [x (graph/hitch graph a node)]
;    (str x)
;    x))
;
;(def score (make-dselector -score))
;
;(defn -t2 [graph node a]
;  (proto/eval-request graph node score (kv/key a)))
;
;(def t2 (make-dselector -t2))
;
;(defn -test3 [graph node v]
;  (str "cat" (proto/eval-request graph node score (proto/construct-selector t2 v))))
;
;(def test3 (make-dselector -test3))
;
;(deftest t1
;         (proto/clear-graph! graph/*graph*)
;         (let [kvs (proto/eval-request graph/*graph* nil keyspace)]
;           (proto/clear! kvs)
;           ;(is (satisfies? proto/ISelector (fns/apply-sel score (kv/key "hi"))))
;           (is (not (proto/dnode? kvs)))
;           (is (= (proto/eval-request graph/*graph* nil t2 "hi") ""))
;           (is (= (proto/eval-request graph/*graph* nil test3 "hi") "cat"))
;           (proto/set-key kvs "hi" :test5)
;           (is (= (proto/eval-request graph/*graph* nil t2 "hi") ":test5"))
;           (is (= (proto/eval-request graph/*graph* nil test3 "hi") "cat:test5"))
;           (proto/set-key kvs "hi" :test6)
;           (is (= (proto/eval-request graph/*graph* nil t2 "hi") ":test6"))
;           (is (= (proto/eval-request graph/*graph* nil test3 "hi") "cat:test6"))
;           ))
;
;(deftest aliases
;         (proto/clear-graph! graph/*graph*)
;         (let [kvs (proto/eval-request graph/*graph* nil keyspace)
;               sfconfig (alias/alias :storefront/config)]
;           ;(proto/clear! kvs)
;           ;(proto/clear! alias)
;           (is (= (proto/eval-request graph/*graph* nil alias/alias :storefront/config) proto/NOT-LOADED))
;           (alias/set-alias graph/*graph* nil :storefront/config (kv/key :test))
;           (is (= (proto/eval-request graph/*graph* nil alias/alias :storefront/config) nil))
;           (proto/set-key kvs :test :test5)
;
;           (is (= (proto/eval-request graph/*graph* nil alias/alias :storefront/config) :test5))
;           #_(fns/hitch-apply  score (kv/key :test))
;
;           (alias/set-alias graph/*graph* nil :storefront/config (proto/construct-selector score (kv/key :test)))
;
;           (is (= (proto/eval-request graph/*graph* nil alias/alias :storefront/config) ":test5"))
;
;         ;(proto/set-key alias "hi3" :test5)
;         ;(is (= (t2 "hi3") ":test5"))
;         ;(is (= (test3 "hi3") "cat:test5"))
;         ;(proto/set-key alias "hi5" :test6)
;         ;(is (= (t2 "hi5") ":test6"))
;         ;(is (= (test3 "hi5") "cat:test6"))
;
;         ))
;
;
;#_(defn ^:export run []
;  (run-tests))
;#_(def hi "hi4")


