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
  (set-key [_ k value]
    (set! store (assoc (or store {}) k value))
    (if-let [n (proto/get-node store/*default-graph* (key ks k))]
      (proto/invalidate! n)))
  (swap-key! [_ f]
    (let [old store]
      (set! store (f old))
      (loop [ks (into #{} (keys old) (keys store))]
        (when (not-empty ks)
          (let [k (first ks)]
            (when (not= (get old k) (get store k))
              (if-let [n (proto/get-node store/*default-graph* (key ks k))]
                (proto/invalidate! n))))
          (recur (rest ks))))))
  (clear! [_]
    (let [old store]
      (set! store {})
      (doseq [alias (keys old)]
        (if-let [n (proto/get-node store/*default-graph* (key ks alias))]
          (proto/invalidate! n)))))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#KVStoreService ")
    (pr-writer store writer opts)))

(defrecord KVStoreServiceSelector [keyspace]
  proto/IDataSelectorStaticRefs
  (selector-dependencies [_] [])
  proto/IDataSelector
  (get-value! [_ args]
    (->KVStoreService {} keyspace)))



(defn keyspace
  ([]
   (->KVStoreServiceSelector nil))
  ([k]
   (->KVStoreServiceSelector k)))

(defrecord KeySelector [ks k]
  proto/IDataSelectorStaticRefs
  (selector-dependencies [_] [ks])
  proto/IDataSelector
  (get-value! [this [alias-service :as args]]
    (proto/get-key alias-service k)))

(defn key
  ([k] (->KeySelector (keyspace) k))
  ([kspace k](->KeySelector (keyspace kspace) k) ))