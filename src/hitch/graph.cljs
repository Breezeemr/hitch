(ns hitch.graph
  (:require [hitch.protocols   :as proto ]
            [hitch.graphs.mutable :as mgraph]
            [hitch.nodes.static-node :as snode]
            ))

(defonce *default-graph* (mgraph/graph))

(def loaded? proto/loaded?)
(defn pure-get
  ([dependency-graph data-selector] (proto/get-node dependency-graph data-selector)))


(defn get-node
  ([dependency-graph data-selector]
    ;(assert (satisfies? proto/ISelectorSingleton data-selector))
   (proto/get-or-create-node dependency-graph data-selector nil nil)))

(defn getn
  ([data-selector]
   #_(if (satisfies? proto/ISelectorSingleton data-selector)
     (proto/get-value (proto/get-or-create-node dependency-graph data-selector nil ))
     (proto/selector-invoke data-selector dependency-graph  (proto/selector-init data-selector dependency-graph nil)))))


(defn hitch-node
  ([dependency-graph data-selector]
   (proto/get-or-create-node dependency-graph data-selector nil nil))
  ([dependency-graph data-selector dependent]
    (assert dependent)
   (proto/get-or-create-node dependency-graph data-selector dependent nil)))


(defn hitch
  ([dependency-graph data-selector]
   (hitch dependency-graph data-selector nil))
  ([dependency-graph data-selector dependent]
   (assert dependent)
   (proto/get-value (hitch-node dependency-graph data-selector dependent))))

(defn cb ([graph callback selector-constructor] (proto/eval-request graph callback selector-constructor))
  ([graph callback selector-constructor a](proto/eval-request graph callback selector-constructor a))
  ([graph callback selector-constructor a b](proto/eval-request graph callback selector-constructor a b))
  ([graph callback selector-constructor a b c](proto/eval-request graph callback selector-constructor a b c))
  ([graph callback selector-constructor a b c d](proto/eval-request graph callback selector-constructor a b c d))
  ([graph callback selector-constructor a b c d f](proto/eval-request graph callback selector-constructor a b c d f))
  ([graph callback selector-constructor a b c d f g](proto/eval-request graph callback selector-constructor a b c d f g))
  ([graph callback selector-constructor a b c d f g h](proto/eval-request graph callback selector-constructor a b c d f g h)))

(defn resolve ([graph ex-dep selector-constructor] (proto/eval-request graph ex-dep selector-constructor))
  ([graph ex-dep selector-constructor a](proto/eval-request graph ex-dep selector-constructor a))
  ([graph ex-dep selector-constructor a b](proto/eval-request graph ex-dep selector-constructor a b))
  ([graph ex-dep selector-constructor a b c](proto/eval-request graph ex-dep selector-constructor a b c))
  ([graph ex-dep selector-constructor a b c d](proto/eval-request graph ex-dep selector-constructor a b c d))
  ([graph ex-dep selector-constructor a b c d f](proto/eval-request graph ex-dep selector-constructor a b c d f))
  ([graph ex-dep selector-constructor a b c d f g](proto/eval-request graph ex-dep selector-constructor a b c d f g))
  ([graph ex-dep selector-constructor a b c d f g h](proto/eval-request graph ex-dep selector-constructor a b c d f g h)))

(defn no-subscribe-resolve ([graph selector-constructor] (proto/eval-request graph nil selector-constructor))
  ([graph  selector-constructor a](proto/eval-request graph  nil selector-constructor a))
  ([graph  selector-constructor a b](proto/eval-request graph nil selector-constructor a b))
  ([graph  selector-constructor a b c](proto/eval-request graph nil selector-constructor a b c))
  ([graph  selector-constructor a b c d](proto/eval-request graph nil selector-constructor a b c d))
  ([graph  selector-constructor a b c d f](proto/eval-request graph nil selector-constructor a b c d f))
  ([graph  selector-constructor a b c d f g](proto/eval-request graph nil selector-constructor a b c d f g))
  ([graph  selector-constructor a b c d f g h](proto/eval-request graph nil selector-constructor a b c d f g h)))


;;; new api
(defn invalidate-level [graph nodes external-invalids]
  ;(prn "nodes " nodes)
  (loop [nodes nodes newvalues (transient {}) external-invalids external-invalids]
    (if-let [node (first nodes)]
      (if  (proto/dnode? node)
        (let [newvalue (proto/make-new-value graph node)
              oldvalue (proto/get-value node)]
          (if (or
                (not= oldvalue newvalue)
                (satisfies? proto/ISelectorReload (proto/get-data-selector node)))
            (recur (rest nodes) (assoc! newvalues node newvalue) external-invalids)
            (recur (rest nodes) newvalues external-invalids)))
        (recur (rest nodes) newvalues (conj! external-invalids node)))
      [(into #{}
         (mapcat
           (fn [[node newvalue]]
             (proto/assign-value! graph node newvalue)
             (eduction cat [(proto/get-dependents node) (proto/get-aliased-by node)])))
         (persistent! newvalues))
       external-invalids])))

(defn invalidate-selectors
  ([graph selectors]
    (prn "temp"  selectors)
   (loop [[nodes external-invalids] (invalidate-level graph (seq (sequence (comp
                                                                             (map #(proto/get-node graph %))
                                                                             (remove nil? ))
                                                                   selectors)) (transient #{}))]
     (if (not-empty nodes)
       (recur (invalidate-level graph nodes external-invalids))
       (persistent! external-invalids)))))

(extend-protocol  proto/ISelectorNode
  default
  (selector-create-node [this graph] (snode/dep-node this)))