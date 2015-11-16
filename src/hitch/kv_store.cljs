(ns hitch.kv-store
  (:refer-clojure :exclude [key])
  (:require [hitch.protocols :as proto]
            [hitch.graph :as store]))

(declare key)
;kv-store
(deftype KVStoreService [^:mutable store ks]
  proto/IKeyStore
  (get-key [_ k]
    (get store k))
  (set-key [this k value]
    (set! store (assoc (or store {}) k value))
    (prn "setkey" k value)
    (hitch.graph/invalidate-selectors [(key ks k)])
    #_(if-let [n (proto/get-node store/*default-graph* (key ks k))]
      (proto/invalidate! n this)))
  (swap-key! [this f]
    (let [old store]
      (set! store (f old))
      (hitch.graph/invalidate-selectors (map #(key ks %) (into #{} (keys old) (keys store))))
      #_(loop [ks (into #{} (keys old) (keys store))]
        (when (not-empty ks)
          (let [k (first ks)]
            (when (not= (get old k) (get store k))
              (if-let [n (proto/get-node store/*default-graph* (key ks k))]
                (proto/invalidate! n this))))
          (recur (rest ks))))))
  (clear! [this]
    (let [old store]
      (set! store {})
      (hitch.graph/invalidate-selectors (map #(key ks %) (keys old)))
      #_(doseq [alias (keys old)]
        (if-let [n (proto/get-node store/*default-graph* (key ks alias))]
          (proto/invalidate! n this)))))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#KVStoreService ")
    (pr-writer store writer opts)))

(defrecord KVStoreServiceSelector [keyspace]
  proto/ISelectorSingleton
  proto/ISelector
  (selector-init [this _] [])
  (selector-ready? [this _ _] true)
  (selector-invoke [this _ _]
    (->KVStoreService {} keyspace)))

(defn keyspace
  ([]
   (->KVStoreServiceSelector nil))
  ([k]
   (->KVStoreServiceSelector k)))

(defrecord KeySelector [ks k]
  proto/ISelector
  (selector-init [this _] (store/hitch-node ks))
  (selector-ready? [this _ _] true)
  (selector-invoke [this alias-service-node _]
    (proto/get-key (proto/get-value alias-service-node) k)))

(defn key
  ([k] (->KeySelector (keyspace) k))
  ([kspace k](->KeySelector (keyspace kspace) k) ))