(ns hitch.graphs.graph-manager
  (:require [hitch.protocol :as proto])
  #?(:clj
     (:import (clojure.lang IDeref)
              (hitch.protocol SelectorValue SelectorUnresolved CommandError))))


(defn- new-value [selector old-value state]
  (let [sv (proto/value selector old-value state)]
    (if (instance? SelectorUnresolved sv)
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
  (let [{:keys [state effect]} (proto/create immutable-graph-selector)
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
                           (let [new-acc (proto/command-step graph acc cmd)]
                             (if (instance? CommandError new-acc)
                               (reduced (assoc new-acc :bad-command cmd))
                               new-acc)))
                         (proto/command-accumulator graph state)
                         cmds)]
    (if (instance? CommandError acc)
      (let [{:keys [bad-command]} acc
            pending-commands (->> cmds
                                  (drop-while #(not (identical? bad-command %)))
                                  first
                                  (into []))]
        [:error graph-node (assoc acc :pending-commands pending-commands)])
      (let [{:keys [effect recalc-child-selectors] new-state :state}
            (proto/command-result graph acc)]
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
      (swap! swap-transact* graph-node-atom cmds r)
      @r)))

(defn update-graph-node!
  "Generic implementation of GraphManager/transact! for atom-based managers."
  [graph-manager graph-node-state-atom cmds tx-watcher]
  (let [[result new-node m old-node] (transact-swapable*! graph-node-state-atom cmds)]
    (case result
      :ok (let [{:keys [effect recalc-external-children]} m
                values {:value-before (:value old-node)
                        :value-after  (:value new-node)}]
            (tx-watcher
              (assoc values
                :graph-manager graph-manager
                :recalc-external-children recalc-external-children
                :effect effect))
            [:hitch.protocol/tx-ok values])
      :error [:hitch.protocol/tx-error m])))

(defn atom-GraphManager
  ;; Tx-watcher gets {:value-before :value-after :graph-manager :effects :recalc-external-children}
  ;; :effects may be nil
  ;; :recalc-external-children may be empty
  ;; The value of things in :recalc-external-children is opaque to the GraphManager,
  ;; but not to the watcher.
  ;; The watcher and all sources of ::proto/child-add ::proto/child-del commands need to
  ;; coordinate on the types expected for external-child.
  ;; rewrite-commands hook is to make this coordination easier (don't always need to delegate transact! from a wrapper)
  ;; Tx-watcher must call effects in same order as it gets them, but not necessarily immediately
  ;; Tx-watcher must signal external children (recalc-external-children) to re-evaluate against the new graph.
  ;; Tx-watcher is not called for initial construction. Don't forget to call the effect from the graph create method!
  [immutable-graph-selector tx-watcher rewrite-commands]
  (let [{:keys [graph-node effect]} (create-graph-node immutable-graph-selector)
        gns (atom graph-node)
        gm  (reify
              IDeref
              (#?(:clj deref :cljs -deref) [_] (:value @gns))

              proto/GraphManager
              (transact! [this cmds]
                (update-graph-node! this gns (rewrite-commands cmds) tx-watcher)))]
    {:graph-manager   gm
     :effect          effect
     :graph-node-atom gns}))

;; Sample watchers

(defn synchronous-watcher
  [{:keys [graph-manager recalc-external-children effects]}]
  (run! #(% @graph-manager) recalc-external-children)
  (when (some? effects)
    (effects graph-manager)))

(comment
  ;; Sketch of how a react manager would work
  ;; May want to batch across multiple transactions.
  (letfn [forceUpdate #(when ^boolean (.isMounted %) (.forceUpdate %))]
    (defn simple-batching-react-watcher
      [{:keys [graph-manager recalc-external-children effects]}]
      (let [{components false fns true} (group-by fn? recalc-external-children)]
        (when (some? fns)
          (run! #(%1 @graph-manager) fns))
        (when (some? components)
          (js/ReactDOM.unstable_batchedUpdates run! forceUpdate components)))
      (when (some? effects)
        (nextTick #(effects graph-manager)))))


  ;; Example setup of a graph manager

  (let [{:keys [graph-manager effect graph-node-atom]}
        (atom-GraphManager
          (hitch.graph.immutable/->ImmutableGraph 1)
          simple-batching-react-watcher identity)]
    ;; Graph setup needs more thought, may need to return a promise of a graph
    ;; manager.
    ;; Problem is graph may have a create effect, and we can't return a useable
    ;; graph manager until that effect runs.
    ;; Alternatively, the graph could handle it itself, buffering commands
    ;; or speculatively transacting them or whatever.
    ;; This will be a problem with a nested graph that needs an IO resource.
    ;; Another approach in nested graph context, add-child to graph and
    ;; wait for it to trigger. When it recalcs, we know graph is ready.

    (when effect
      (effect graph-manager))

    ;; Only commands allowed are
    ;;     [::proto/child-add sel external-child]
    ;;     [::proto/child-del sel external-child]
    ;;     [::proto/command sel command]

    (proto/transact! graph-manager
      [[::proto/child-add
        (->SomeSelector)
        (fn the-hook [v]
          (proto/transact! graph-manager
            [[::proto/child-del (->SomeSelector) the-hook]])
          (. js/console (log (get v (->SomeSelector)))))]])))
