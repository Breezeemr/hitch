(ns hitch.selectors.kv-store
  (:refer-clojure :exclude [get get-in])
  (:require [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.tracking.halt :as halt]
            [hitch.selector #?(:cljs :refer-macros :default :refer) [defselector]]))

(defonce ^:private NOT-FOUND #?(:cljs (js/Object.) :default (Object.)))

(defselector get [graph sel k]
  (let [found (clojure.core/get @(graph/select-sel! graph sel) k NOT-FOUND)]
    (if (identical? found NOT-FOUND)
      (halt/halt!)
      found)))

(defselector get-in [graph keystore path]
  (let [found (clojure.core/get-in @(graph/select-sel! graph keystore) path NOT-FOUND)]
    (if (identical? found NOT-FOUND)
      (halt/halt!)
      found)))

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
  (into dirty-sels
    (comp
      (drop 1)
      (mapcat deps))
    (reductions conj [] dirty-path)))

(defrecord KVStoreServiceSelector [keyspace]
  proto/InformedSelector
  proto/SilentSelector

  proto/StatefulSelector
  (create [selector]
    (proto/->StateEffect {:deps {} :value NOT-FOUND} nil nil))
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
        ::proto/child-del (-> (update acc :deps update-deps arg false)
                              (update :dirty-sels conj arg)))))
  (command-result [s acc]
    (proto/->StateEffect (dissoc acc :dirty-sels) nil
      (when-not (identical? (:value acc) NOT-FOUND)
        (:dirty-sels acc))))

  proto/Selector
  (value [this graph state]
    (let [v (:value state)]
      (if (identical? v NOT-FOUND)
        (proto/->SelectorUnresolved nil)
        (proto/->SelectorValue v nil)))))

(def keyspace ->KVStoreServiceSelector)

