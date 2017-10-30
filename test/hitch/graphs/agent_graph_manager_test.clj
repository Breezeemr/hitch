(ns hitch.graphs.agent-graph-manager-test
  (:require [clojure.test :refer :all]
            [hitch.protocol :as hp]
            [hitch.oldprotocols :as op]
            [hitch.graphs.immutable :as im]
            [hitch.graphs.agent-graph-manager :as agm]))


(defrecord Constant [v]
  hp/Selector
  (value [_ _ _]
    (hp/->SelectorValue v nil)))

(defrecord Variable [name]
  hp/Selector
  (value [s sv state] state)
  hp/CommandableSelector
  (command-accumulator [s old-state] old-state)
  (command-step [s accumulator command]
    (case (first command)
      :set! (second command)
      (hp/map->CommandError {:accumulator accumulator
                             :error       "Unrecognized Command"})))
  (command-result [s accumulator]
    (hp/->StateEffect accumulator nil nil)))

(defrecord SelVec [parents]
  hp/Selector
  (value [_ sv _]
    (let [v (reduce
              (fn [r parent]
                (let [x (get sv parent ::not-found)]
                  (if (= x ::not-found)
                    (reduced ::not-found)
                    (conj r x))))
              [] parents)]
      (if (= v ::not-found)
        (hp/->SelectorUnresolved parents)
        (hp/->SelectorValue v parents)))))

(defrecord EffectSel [on-command-effect]
  hp/CommandableSelector
  (command-accumulator [s old-state] old-state)
  (command-step [s accumulator command] accumulator)
  (command-result [s accumulator]
    (hp/->StateEffect accumulator on-command-effect nil)))

(defn watch-transact-sync [{:keys [oob-request-data] :as x}]
  (when-some [p (::promise oob-request-data)]
    (->> (update x :oob-request-data dissoc ::promise)
         (deliver p))))

(defn transact-sync [agm cmds oob]
  (let [r (promise)]
    (when (agm/transact-agent-graph-manager! agm cmds (assoc oob ::promise r))
      r)))

(deftest use-agent-graph-manager
  (let [graph                 (im/->ImmutableGraph 1)
        gm-errors             (atom [])
        gm-errors-handler     (fn [a e] (swap! gm-errors conj e))
        effect-errors         (atom [])
        effect-errors-handler (fn [a e] (swap! effect-errors conj e))
        notify-errors         (atom [])
        notify-errors-handler (fn [a e] (swap! notify-errors conj e))
        agm                   (agm/agent-graph-manager graph
                                gm-errors-handler effect-errors-handler
                                notify-errors-handler)]

    (agm/watch-agent-graph-manager agm watch-transact-sync)

    (try
      (let [ext-child    (reify op/ExternalDependent
                           (-change-notify [_] nil))
            oob-data     {:OOB true}
            vec-selector (->SelVec [(->Constant 1) (->Constant 2) (->Constant 3)])
            {:keys [running?
                    tx-id
                    graph-before
                    graph
                    error-count
                    oob-request-data
                    effect
                    recalc-external-children
                    observable-changed-selector-values
                    ] :as x} @(transact-sync agm [[::hp/child-add vec-selector ext-child]]
                          oob-data)]
        (prn (keys x))
        (is (true? running?))
        (is (= tx-id 1))
        (is (= oob-request-data oob-data))
        (is (= recalc-external-children #{ext-child}))
        (is (= observable-changed-selector-values {vec-selector [1 2 3]}))
        (is (zero? error-count))
        (is (nil? effect))
        (is (= (get (:value graph) vec-selector) [1 2 3]))
        (is (= (get (:value graph-before) vec-selector :not-found) :not-found)))

      (finally
        (agm/stop-and-flush-agent-graph-manager! agm nil)))))
