(ns hitch.test-common
  (:require [hitch.protocol :as hp]))

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

;; Machine which will var-reset its vars with their own :v param
(defrecord EchoMachine []
  hp/Machine
  (apply-machine-commands [_ g s commands]
    (reduce
      (fn [r [cmd sel]]
        (case cmd
          ::hp/child-add (assoc-in r [:var-reset sel] (:v sel))
          ::hp/child-del r
          ::hp/parent-value-change r))
      commands)))

;; The value of an EchoVar
(defrecord EchoVar [v]
  hp/Var
  (machine-selector [_] (->EchoMachine)))

(defrecord FnMachine []
  hp/Machine
  (apply-machine-commands [_ g s commands]
    (reduce
      (fn [r [cmd f]]
        (case cmd
          :do (f r g s)
          ::hp/child-add r
          ::hp/child-del r
          ::hp/parent-value-change r))
      {}
      commands)))

(defrecord FnVar [id]
  hp/Var
  (machine-selector [_] (->FnMachine)))
