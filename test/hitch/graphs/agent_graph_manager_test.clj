(ns hitch.graphs.agent-graph-manager-test
  (:require [clojure.test :refer :all]
            [hitch.protocol :as hp]
            [hitch.oldprotocols :as op]
            [hitch.graph :as h]
            [hitch.graphs.immutable :as im]
            [hitch.graphs.agent-graph-manager :as agm]
            [hitch.test-common :refer :all]))

(defrecord EffectSel [on-create-effect]
  hp/StatefulSelector
  (create [s]
    (hp/->StateEffect :created on-create-effect nil))
  (destroy [s state] nil)
  hp/Selector
  (value [s g state]
    (hp/->SelectorValue state nil))
  hp/CommandableSelector
  (command-accumulator [s old-state] {:state old-state})
  (command-step [s accumulator command]
    (apply assoc accumulator command))
  (command-result [s accumulator]
    (hp/->StateEffect (:state accumulator) (:effect accumulator) nil)))

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
      (let [ext-child         (reify op/ExternalDependent
                                (-change-notify [_] nil))
            oob-data          {:OOB true}
            created?          (promise)
            commanded?        (promise)
            effect-sel-effect (fn self [g]
                                (deliver created? true)
                                (h/apply-commands g
                                  [[(->EffectSel self)
                                    [:effect (fn [_] (deliver commanded? true))]]]))
            effect-sel        (->EffectSel effect-sel-effect)
            vec-selector      (->SelVec [(->Constant 1) (->Constant 2) (->Constant 3)
                                         effect-sel])
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
        (is (true? running?))
        (is (= tx-id 1))
        (is (= oob-request-data oob-data))
        (is (= recalc-external-children #{ext-child}))
        (is (= observable-changed-selector-values {vec-selector [1 2 3 :created]}))
        (is (zero? error-count))
        (is (identical? effect effect-sel-effect))
        (is (= (get (:value graph) vec-selector) [1 2 3 :created]))
        (is (= (get (:value graph-before) vec-selector :not-found) :not-found))
        (is (true? (await-for 1000 (:gm-agent agm) (:effect-agent agm) (:notify-agent agm)))
          "Agents should eventually drain")
        (is (deref created? 1 false) "Creation effect should run")
        (is (deref commanded? 1 false) "Command effect should run"))

      (finally
        @(agm/stop-and-flush-agent-graph-manager! agm nil)
        (binding [*out* *err*]
          (when-not (empty? @gm-errors)
            (println "Graph Manager Errors:")
            (run! prn @gm-errors))
          (when-not (empty? @effect-errors)
            (println "Effect Errors:")
            (run! prn @effect-errors))
          (when-not (empty? @notify-errors)
            (println "Notify Errors:")
            (run! prn @notify-errors)))))))
