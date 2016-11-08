(ns hitch.graphs.graph-manager
  (:require [hitch.protocol :as hp]
            [hitch.oldprotocols :as op])
  #?(:clj
     (:import (clojure.lang IDeref ILookup))))


(defn- new-value [selector old-value state]
  (let [sv (hp/value selector old-value state)]
    (if (hp/selector-unresolved? sv)
      old-value
      (:value sv))))

(defn create-graph-node
  "Return a graph-node and an effect function (if any) that must be run to
  initalize the graph.

  A graph-node is a \"live\" graph selector. It is a map of :graph (the graph
  selector), :state (the graph's internal state), and :value (a map of selectors
  to known values inside the graph.)

  Because a graph selector may have a StatefulSelector create method, there may
  be an effect function which must be called before the graph is useable.
  It is your responsiblity to wrap graph-node in something that implements
  TransactableGraphManager and invoke the effect with it as its argument."
  [immutable-graph-selector]
  (let [{:keys [state effect]} (hp/create immutable-graph-selector)
        graph-node {:graph immutable-graph-selector
                    :state state
                    :value (new-value immutable-graph-selector nil state)}]
    {:graph-node graph-node
     :effect     effect}))

(defn apply-graph-node-commands
  "Apply commands to a graph node and return either:

      [:ok new-graph-node {:effect effect-fn :recalc-external-children [...]}]
      [:error hitch.protocol/CommandError-instance]"
  [{:keys [graph state] old-value :value :as graph-node} cmds]
  ;; TODO: Run the reduction manually to allow resumption via CommandError
  ;; and a dynamic var
  (let [acc (reduce
              (fn [acc cmd]
                (let [new-acc (hp/command-step graph acc cmd)]
                  (if (hp/command-error? new-acc)
                    (reduced (assoc new-acc :bad-command cmd :accumulator acc))
                    new-acc)))
              (hp/command-accumulator graph state)
              cmds)]
    (if (hp/command-error? acc)
      (let [{:keys [bad-command]} acc
            pending-commands (->> cmds
                                  (drop-while #(not (identical? bad-command %)))
                                  first
                                  (into []))]
        [:error graph-node (assoc acc :pending-commands pending-commands)])
      (let [{:keys [effect recalc-child-selectors] new-state :state}
            (hp/command-result graph acc)]
        [:ok (assoc graph-node :state new-state
                               :value (new-value graph old-value new-state))
         {:effect effect :recalc-external-children recalc-child-selectors}]))))

(letfn [(swap-transact* [graph-node cmds v-tx-res]
          (let [[_ new-gm :as res] (apply-graph-node-commands graph-node cmds)]
            (vreset! v-tx-res (conj res graph-node))
            new-gm))]
  (defn transact-swapable*!
    "Given an atom which has a graph-node in it, and commands to apply to the
    node, atomically swap the new graph-node value into the atom and return
    a transaction result which is like the result of transact-graph-node
    but with the old graph-node state conj-ed on to the end.

    This is an internal function but it is useful for implementing
    GraphManagers."
    [graph-node-atom cmds]
    (let [r (volatile! nil)]
      (swap! graph-node-atom swap-transact* cmds r)
      @r)))

(defn update-graph-node!
  "Generic implementation of GraphManager/transact! for atom-based managers."
  [graph-node-state-atom cmds]
  (let [[result new-node m old-node] (transact-swapable*! graph-node-state-atom cmds)]
    (case result
      :ok (let [values {:value-before (:value old-node)
                        :value-after  (:value new-node)}]
            [:hitch.protocol/tx-ok values m])
      :error [:hitch.protocol/tx-error m])))

(defn synchronous-effect-runner [gm effect]
  (effect gm))

(defn atom-GraphManager
  ;; Every object in recalc-external-children must implement -change-notify,
  ;; which will be called immediately after successful tx
  ;; run-effect! will be called with an effect function and a graph manager.
  ;; It must ensure effect gets called.
  ([immutable-graph-selector]
   (atom-GraphManager immutable-graph-selector synchronous-effect-runner))
  ([immutable-graph-selector run-effect!]
   (let [{:keys [graph-node effect]} (create-graph-node immutable-graph-selector)
         gns (atom graph-node)
         gm  (reify
               IDeref
               #?(:clj  (deref [_] (:value @gns))
                  :cljs (-deref [_] (:value @gns)))

               ;; Here only for compatiblity, should probably be removed
               ILookup
               #?@(:clj  [(valAt [this sel] (.valAt ^ILookup @this sel nil))
                          (valAt [this sel nf] (.valAt ^ILookup @this sel nf))]
                   :cljs [(-lookup [this sel] (-lookup @this sel nil))
                          (-lookup [this sel nf] (-lookup @this sel nf))])

               hp/GraphManager
               (transact! [this cmds]
                 (when-not (empty? cmds)
                   (let [tx-result (update-graph-node! gns cmds)]
                     (if (= (first tx-result) ::hp/tx-ok)
                       (let [{:keys [effect recalc-external-children]} (peek tx-result)]
                         (when (some? effect) (run-effect! this effect))
                         (run! op/-change-notify recalc-external-children)
                         (pop tx-result))
                       tx-result))))

               op/IDependencyGraph
               (update-parents [this child add rm]
                 (when-not (and (empty? add) (empty? rm))
                   (hp/transact! this
                     [[:hitch.graphs.immutable/child-adds-dels child add rm]]))
                 nil)

               (apply-commands [this sel+cmd-pairs]
                 (hp/transact! this
                   (into []
                     (map (fn [[s cmd]] [::hp/command s cmd]))
                     sel+cmd-pairs))
                 nil))]
     (when (some? effect) (run-effect! effect gm))
     gm)))
