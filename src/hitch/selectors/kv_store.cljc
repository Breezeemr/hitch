(ns hitch.selectors.kv-store
  (:refer-clojure :exclude [get get-in])
  (:require [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.tracking.halt :as halt]
            [hitch.selector #?(:cljs :refer-macros :default :refer) [defselector]]))

(defonce ^:private NOT-FOUND #?(:cljs (js/Object.) :default (Object.)))

(defselector get [graph sel k]
  (clojure.core/get @(graph/select-sel! graph sel) k))

(defselector get-in [graph keystore path]
  (clojure.core/get-in @(graph/select-sel! graph keystore) path))

(defn- update-deps [deps selector #?(:cljs ^boolean add? :default add?)]
  (condp instance? selector
    get-selector
    (if add?
      (update deps [(:k selector)] (fnil conj #{}) selector)
      (update deps [(:k selector)] disj selector))
    get-in-selector
    (if add?
      (update deps (:path selector) (fnil conj #{}) selector)
      (update deps (:path selector) disj selector))

    ;; Unrecognized selectors will not be updated
    deps))

(defn- add-dirty-sels [dirty-sels deps dirty-path]
  (-> (into dirty-sels
        (comp
          (drop 1)
          (mapcat deps))
        (reductions conj [] dirty-path))
      (into
        (comp
          (let [dpc (count dirty-path)]
            (take-while (fn [[k]]
                          (and (>= (count k) dpc)
                            (= dirty-path (subvec k 0 dpc))))))
          (mapcat val))
        (subseq deps > dirty-path))))

(letfn [(cmp [^objects vec-b+maxi i a]
          ;; INVARIANT: vec-a is longer than or equal-len vec-b
          ;; INVARIANT: vec-a and vec-b are len >= 1
          (let [vec-b (aget vec-b+maxi 0)
                maxi  (aget vec-b+maxi 1)
                b     (nth vec-b i)
                diff  (compare a b)]
            (if (zero? diff)
              (if (== i maxi)
                (reduced vec-b+maxi)
                vec-b+maxi)
              (reduced diff))))
        (finalize [x default]
          (if (number? x)
            x
            default))]
  (defn- depth-first-path-compare [vec-a vec-b]
    (let [cva (count vec-a)
          cvb (count vec-b)]
      (cond
        (zero? cva) (if (zero? cvb) 0 -1)
        (zero? cvb) (if (zero? cva) 0 1)
        (>= cva cvb)
        (-> (reduce-kv cmp
              (doto (object-array 2)
                (aset 0 vec-b)
                (aset 1 ^Object (dec (count vec-b))))
              vec-a)
            (finalize (if (== cva cvb) 0 1)))
        :else
        (-> (reduce-kv cmp
              (doto (object-array 2)
                (aset 0 vec-a)
                (aset 1 ^Object (dec (count vec-a))))
              vec-b)
            (finalize 1)
            (-))))))

(defrecord KVStoreServiceSelector [keyspace]
  proto/InformedSelector
  proto/SilentSelector

  proto/StatefulSelector
  (create [selector]
    (proto/->StateEffect {:deps (sorted-map-by depth-first-path-compare)
                          :value NOT-FOUND} nil nil))
  (destroy [selector state]
    nil)

  proto/CommandableSelector
  (command-accumulator [s state]
    (assoc state :dirty-sels #{}))
  (command-step [s acc command]
    (let [[cmd arg] command]
      (case cmd
        ;; [:set-value indexed]
        ::reset (-> (assoc acc :value arg)
                    (update :dirty-sels into cat (vals (:deps acc))))
        ;; [::assoc-in path value]
        ::assoc-in (-> (update acc :value assoc-in arg (peek command))
                       (update :dirty-sels add-dirty-sels (:deps acc) arg))
        ::proto/child-add (-> (update acc :deps update-deps arg true)
                              (update :dirty-sels conj arg))
        ::proto/child-del (update acc :deps update-deps arg false))))
  (command-result [s acc]
    (proto/->StateEffect (dissoc acc :dirty-sels) nil (:dirty-sels acc)))

  proto/Selector
  (value [this graph state]
    (let [v (:value state)]
      (if (identical? v NOT-FOUND)
        (proto/->SelectorUnresolved nil)
        (proto/->SelectorValue v nil)))))

(def keyspace ->KVStoreServiceSelector)

