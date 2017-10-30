(ns hitch.graphs.agent-graph-manager
  "A graph manager designed for use from the other end of a connection."
  (:require [hitch.protocol :as hp]
            [hitch.oldprotocols :as op]
            [hitch.graphs.graph-manager :as gm])
  (:import (clojure.lang ILookup IDeref)))

(defn- gm-agent-transact-runner [gm-state commands oob-request-data]
  (cond
    (not (:running? gm-state))
    (assoc gm-state :oob-request-data oob-request-data)

    (= commands :stop!)
    (let [{{:keys [graph state]} :graph} gm-state
          effect (hp/destroy graph state)]
      (when effect
        (effect nil))
      {:running?         false
       :graph-before     (:graph gm-state)
       :tx-id            (:tx-id gm-state)
       :error-count      (:error-count gm-state)
       :oob-request-data oob-request-data})

    :else
    (let [{graph-node :graph} gm-state
          [status :as r] (gm/apply-graph-node-commands graph-node commands)]
      (case status
        :ok
        (let [[_ graph-node' {:keys [effect recalc-external-children]}] r]
          {:running?                 (:running? gm-state)
           :graph-before             graph-node
           :graph                    graph-node'
           :tx-id                    (unchecked-inc (:tx-id gm-state))
           :error-count              (:error-count gm-state)
           :effect                   effect
           :recalc-external-children recalc-external-children
           :oob-request-data         oob-request-data})
        :error
        (let [[_ error] r]
          {:running?         (:running? gm-state)
           :graph-before     (:graph-before gm-state)
           :graph            graph-node
           :tx-id            (:tx-id gm-state)
           :error            error
           :error-count      (unchecked-inc (:error-count gm-state))
           :oob-request-data oob-request-data})))))

(defn- gm-agent-transact! [gm-agent commands oob-request-data]
  (if (:running? @gm-agent)
    (do
      (send gm-agent gm-agent-transact-runner commands oob-request-data)
      true)
    false))

;; Graph manager for effect functions ONLY
(deftype EffectGraphManager [gm-agent oob]
  IDeref
  (deref [_] (:value (:graph @gm-agent)))

  ;; Here only for compatibility, should probably be removed
  ILookup
  (valAt [this sel] (.valAt ^ILookup @this sel nil))
  (valAt [this sel nf] (.valAt ^ILookup @this sel nf))

  hp/GraphManager
  (transact! [this cmds]
    (when-not (empty? cmds)
      (gm-agent-transact! gm-agent cmds oob))
    nil)

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
    nil))

(defn- notify-runner [prev-notify-tx-id tx-id ext-children]
  (run! op/-change-notify ext-children)
  tx-id)

(defn- effect-runner [prev-effect-tx-id tx-id effect gm-agent]
  (let [gm (->EffectGraphManager gm-agent {::effect-from-tx-id tx-id})]
    (effect gm))
  tx-id)

(defn- effect+notify-watcher [effect-agent notify-agent]
  (fn [key gm-agent prev-state new-state]
    (when-some [ext-children (not-empty (:recalc-external-children new-state))]
      (send-off notify-agent notify-runner (:tx-id new-state) ext-children))
    (when-some [effect (:effect new-state)]
      (send-off effect-agent effect-runner (:tx-id new-state) effect gm-agent))))


;; PUBLIC INTERFACE

(defn agent-graph-manager
  "Return a graph-manager which is agent-based.

  Returns a map of the graph agent, effect-agent, and notify-agent.

  The graph agent holds the effect of the last transaction or error.
  The notify and effect agents hold the id of the last transaction for which
  the effects were run or the notifies were transmitted.

  Use `transact-agent-graph-manager!` on the return value to send transactions,
  `watch-agent-graph-manager` to watch the agent's transaction results, and
  `stop-and-flush-agent-graph-manager!` to stop the graph manager."
  [immutable-graph-selector gm-error-handler
   effect-agent-error-handler notify-agent-error-handler]
  (let [{:keys [graph-node effect]} (gm/create-graph-node immutable-graph-selector)
        gm-agent     (agent {:graph       graph-node
                             :running?    true
                             :tx-id       0
                             :error-count 0}
                       :error-handler gm-error-handler :error-mode :continue)
        effect-agent (agent 0 :error-handler effect-agent-error-handler
                       :error-mode :continue)
        notify-agent (agent 0 :error-handler notify-agent-error-handler
                       :error-mode :continue)]
    (add-watch gm-agent "effect+notify-watcher"
      (effect+notify-watcher effect-agent notify-agent))
    (when effect
      (effect-runner -1 0 effect gm-agent))

    {:gm-agent     gm-agent
     :effect-agent effect-agent
     :notify-agent notify-agent
     :watches      (atom #{})}))

(defn watch-agent-graph-manager
  "Add a watch function on the graph-manager's agent. The function will be called
  with the latest transaction result.

  Transaction results look like this after a success:

      {:running?                 true
       :graph-before             previous-graph-node
       :graph                    current-graph-note
       :tx-id                    id-incremented-by-successful-txs
       :error-count              count-incremented-by-tx-errors
       :effect                   effect-fn-if-any
       :recalc-external-children set-of-ext-children-whose-subscribed-vals-changed
       :oob-request-data         oob-request-data-from-the-transact-request}

  Transaction results look like this after an error:

      {:running?         true
       :graph-before     previous-graph-node  ;Unchanged from previous successful tx
       :graph            current-graph-node   ;Unchanged from previous successful tx
       :tx-id            tx-id                ;Unchanged from previous successful tx
       :error            error                ;CommandError that caused the error
       :error-count      incremented-by-errors
       :oob-request-data oob-request-data-from-the-transact-request
       }

  After the graph is stopped, results look like this:

       {:running?         false
        :graph-before     last-graph-value-before-destruction
        :tx-id            last-successful-tx-id
        :error-count      last-error-count
        :oob-request-data oob-request-data-updates-for-every-request
        }

  You can use oob-request-data to retire transactions requested while the graph
  was running but which arrived at the agent after the graph stops.

  Returns an opaque id you can use to unwatch with
  `unwatch-agent-graph-manager`."
  [{:keys [gm-agent watches]} watch-fn]
  (let [watch-id (gensym "graph-agent-watch_")]
    (swap! watches conj watch-id)
    (add-watch gm-agent watch-id (fn [_ _ _ new-state] (watch-fn new-state)))
    watch-id))

(defn unwatch-agent-graph-manager
  "Removes a watch added with `watch-agent-graph-manager`. Returns true if the
  watch was removed; false if there was no watch with the supplied id."
  [{:keys [gm-agent watches]} watch-id]
  (let [has-watch (contains? @watches watch-id)]
    (when has-watch
      (remove-watch gm-agent watch-id))
    has-watch))

(defn transact-agent-graph-manager!
  "Accepts a vector of commands and optional out-of-band data and sends both
  to an agent-based graph manager.

  To see the transaction result, you must have attached a watcher to the graph
  manager first with `watch-agent-graph-manager`. You can use oob-request-data
  to correlate submitted transactions with transaction results.

  Returns true if the transaction was scheduled, else false. Note: A scheduled
  transaction may still not be run if the graph manager was stopped before
  the transaction could reach it. However, it will always get its
  oob-request-data back."
  [{:keys [gm-agent]} commands oob-request-data]
  (gm-agent-transact! gm-agent commands oob-request-data))

(defn stop-and-flush-agent-graph-manager!
  "Will ask the agent-based graph-manager to stop all transactions then wait
  until all pending requests are done and all agent queues are emptied, or
  until timeout is reached if non-nil.

  The stop is scheduled with other transactions, so there may be other pending
  transactions in front of it which will run, or other transactions may get
  scheduled behind it which will not run.

  The stop command destroys the graph (calls hp/destroy on all stateful
  selectors), rendering the graph unusable for future transactions.

  The graph, effect, and notify agents should not be used after this command.


  Returns nil if graph manager is already stopped; true if all agents are
  completely flushed and all watches removed; or false if the timeout was
  hit first (watches were not removed)."
  [{:keys [gm-agent effect-agent notify-agent watches]} timeout-ms]
  (when (gm-agent-transact! gm-agent :stop! nil)
    (let [finished-waiting? (if (some? timeout-ms)
                             (await-for timeout-ms gm-agent effect-agent notify-agent)
                             (do (await gm-agent effect-agent notify-agent)
                                 true))]
      (when finished-waiting?
        (remove-watch gm-agent "effect+notify-watcher")
        (run! #(remove-watch gm-agent %) @watches)
        (reset! watches []))
      finished-waiting?)))
