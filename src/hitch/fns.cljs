(ns hitch.fns
  (:require [hitch.protocols :as proto]
            [hitch.graph :as store]))

(defrecord FN1 [f a]
  proto/IDataSelectorDynamicRefs
  proto/IDataSelector
  (get-value! [this args]
    #_(binding [proto/*dependent* (proto/get-node store/*store* this)] )
    (f a)))
(defrecord FN2 [f a b]
  proto/IDataSelectorDynamicRefs
  proto/IDataSelector
  (get-value! [this args]
    (f a b)))
(defrecord FN3 [f a b c]
  proto/IDataSelectorDynamicRefs
  proto/IDataSelector
  (get-value! [this args]
    (f a b c)))
(defrecord FN4 [f a b c d]
  proto/IDataSelectorDynamicRefs
  proto/IDataSelector
  (get-value! [this args]
    (f a b c d)))
(defrecord FN5 [f a b c d e]
  proto/IDataSelectorDynamicRefs
  proto/IDataSelector
  (get-value! [this args]
    (f a b c d e)))
(defrecord FNrest [f a b c d e rest]
  proto/IDataSelectorDynamicRefs
  proto/IDataSelector
  (get-value! [this args]
    (apply f a b c d (conj rest e))))

(defn apply-sel
  ([data-selector] data-selector)
  ([f a] (FN1. f a))
  ([f a b] (FN2. f a b))
  ([f a b c] (FN3. f a b c))
  ([f a b c d] (FN4. f a b c d))
  ([f a b c d e] (FN5. f a b c d e))
  ([f a b c d e & rest] (FNrest. f a b c d e rest)))

#_(defn graph-hitch-dapply
  ([graph dependent data-selector]
   (store/hitch graph data-selector dependent))
  ([graph dependent f a] (graph-hitch-dapply graph dependent (FN1. f a)))
  ([graph dependent f a b] (graph-hitch-dapply graph dependent (FN2. f a b)))
  ([graph dependent f a b c] (graph-hitch-dapply graph dependent (FN3. f a b c)))
  ([graph dependent f a b c d] (graph-hitch-dapply graph dependent (FN4. f a b c d)))
  ([graph dependent f a b c d e] (graph-hitch-dapply graph dependent (FN5. f a b c d e)))
  ([graph dependent f a b c d e & rest] (graph-hitch-dapply graph dependent (FNrest. f a b c d e rest))))

#_(defn hitch-dapply
  ([dependent data-selector]
   (store/hitch store/*default-graph* data-selector dependent))
  ([dependent f a] (hitch-dapply dependent (FN1. f a)))
  ([dependent f a b] (hitch-dapply dependent (FN2. f a b)))
  ([dependent f a b c] (hitch-dapply dependent (FN3. f a b c)))
  ([dependent f a b c d] (hitch-dapply dependent (FN4. f a b c d)))
  ([dependent f a b c d e] (hitch-dapply dependent (FN5. f a b c d e)))
  ([dependent f a b c d e & rest] (hitch-dapply dependent (FNrest. f a b c d e rest))))

#_(defn hitch-apply
  ([data-selector] (store/hitch store/*default-graph* data-selector proto/*dependent*))
  ([f a] (hitch-apply (FN1. f a)))
  ([f a b] (hitch-apply (FN2. f a b)))
  ([f a b c] (hitch-apply (FN3. f a b c)))
  ([f a b c d] (hitch-apply (FN4. f a b c d)))
  ([f a b c d e] (hitch-apply (FN5. f a b c d e)))
  ([f a b c d e & rest] (hitch-apply (FNrest. f a b c d e rest))))

