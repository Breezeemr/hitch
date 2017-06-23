(ns hitch.selectors.kv-store
  (:refer-clojure :exclude [get-in])
  (:require [hitch.oldprotocols :as oldproto]
            [hitch.protocol :as proto]
            [hitch.graph :as graph]
            [hitch.tracking.halt :as halt]
            [hitch.selector #?(:cljs :refer-macros :default :refer) [defselector]]))

(defonce ^:private NOT-FOUND #?(:cljs (js/Object.) :default (Object.)))

(defselector keystore-get [graph sel k]
  (let [found (get @(graph/select-sel! graph sel) k NOT-FOUND)]
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
    keystore-get-selector
    (if add?
      (update deps [(:k selector)] (fnil conj #{}) selector)
      (update deps [(:k selector)] disj selector))
    get-in-selector
    (if add?
      (update deps (:path selector) (fnil conj #{}) selector)
      (update deps (:path selector) disj selector))))

(defn- add-dirty-sels [dirty-sels deps dirty-path]
  (let [cpath (count dirty-path)]
    (into dirty-sels (comp
                       (take-while (fn [kv]
                                     (let [path (key kv)]
                                       (or (= path dirty-path)
                                         (and (< cpath (count path))
                                           (= (subvec path 0 cpath)
                                             dirty-path))))))
                       (mapcat val))
      (subseq dirty-sels >= dirty-path))))


(defrecord KVStoreServiceSelector [keyspace]
  proto/InformedSelector
  proto/SilentSelector

  proto/StatefulSelector
  (create [selector]
    (proto/->StateEffect {:deps (sorted-map) :value oldproto/NOT-FOUND-SENTINEL} nil nil))
  (destroy [selector state]
    nil)

  proto/CommandableSelector
  (command-accumulator
    [s state] (assoc state :dirty-sels #{}))
  (command-step [s acc command]
    (let [[cmd arg] command]
      (case cmd
        ;; [:clear]
        :clear (-> (assoc acc :value oldproto/NOT-FOUND-SENTINEL)
                   (update :dirty-sels into cat (vals (:deps acc))))
        ;; [:set-value indexed]
        :set-value (-> (assoc acc :value arg)
                       (update :dirty-sels into cat (vals (:deps acc))))
        ;; [::assoc-in path value]
        ::assoc-in (-> (update acc :value assoc-in arg (peek command))
                       (update :dirty-sels add-dirty-sels (:deps acc) arg))
        ::proto/child-add (-> (update acc :deps update-deps arg true)
                              (update :dirty-sels conj arg))
        ::proto/child-del (-> (update acc :deps update-deps arg false)
                              (update :dirty-sels conj arg)))))
  (command-result [s acc]
    (proto/->StateEffect (dissoc acc :dirty-sels) nil (:dirty-sels acc)))

  proto/Selector
  (value [this graph state]
    (let [v (:value state)]
      (if (identical? v oldproto/NOT-FOUND-SENTINEL)
        (proto/->SelectorUnresolved nil)
        (proto/->SelectorValue v nil)))))

(def keyspace ->KVStoreServiceSelector)

