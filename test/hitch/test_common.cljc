(ns hitch.test-common
  #?(:clj (:import (java.io Writer)))
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
        (hp/->SelectorUnresolved (set parents))
        (hp/->SelectorValue v (set parents))))))

;; Machine which will var-reset its vars with their own :v param
(defrecord EchoMachine []
  hp/Machine
  (apply-machine-commands [_ g s commands]
    (reduce
      (fn [r [cmd sel [varcmd v]]]
        (case cmd
          ::hp/var-command
          (case varcmd
            :reset! (assoc-in r [:var-reset sel] v))
          ::hp/child-add (assoc-in r [:var-reset sel] (:initial sel))
          ::hp/child-del r
          ::hp/parent-value-change r))
      {}
      commands)))

;; The value of an EchoVar
(defrecord EchoVar [initial]
  hp/Var
  (machine-selector [_] (->EchoMachine)))

(defrecord LogMachine [log-volatile]
  hp/StatefulSelector
  (create [_]
    (vswap! log-volatile conj [:create])
    (hp/->StateEffect {:count 0} nil nil))
  (destroy [_ s]
    (vswap! log-volatile conj [:destroy (:count s)])
    nil)
  hp/Machine
  (apply-machine-commands [_ g {:keys [state] :as sn} commands]
    (vswap! log-volatile conj [:commands (:count state) commands])
    (reduce (fn [r [type parent-sel [subcmd f]]]
              (case type
                ::hp/var-command
                (case subcmd
                  :fn (f r g sn)
                  :log (let [lv (f r g sn)]
                         (vswap! log-volatile conj [:log (:count state) lv])
                         r))

                ::hp/parent-value-change
                (do
                  (vswap! log-volatile conj [:new-parent-value (:count state) parent-sel (get g parent-sel ::absent)])
                  r)

                r))
      {:state (update state :count inc)}
      commands)))

(defrecord LogVar [log-volatile]
  hp/Var
  (machine-selector [_] (->LogMachine log-volatile)))

(defrecord LogVar2 [log-volatile]
  hp/Var
  (machine-selector [_] (->LogMachine log-volatile)))

#?(:cljs
   (extend-protocol IPrintWithWriter
     LogMachine
     (-pr-writer [o writer opts]
       (doto writer
         (-write "#<LogMachine ")
         (-write (str (hash (:log-volatile o))))
         (-write ">")))

     LogVar
     (-pr-writer [o writer opts]
       (doto writer
         (-write "#<LogVar ")
         (-write (str (hash (:log-volatile o))))
         (-write ">")))

     LogVar2
     (-pr-writer [o writer opts]
       (doto writer
         (-write "#<LogVar2 ")
         (-write (str (hash (:log-volatile o))))
         (-write ">")))))

#?(:clj
   (defmethod print-method LogMachine [c, ^Writer w]
     (doto w
       (.write "#<LogMachine ")
       (.write (str (hash (:log-volatile c))))
       (.write ">"))))

#?(:clj
   (defmethod print-method LogVar [c, ^Writer w]
     (doto w
       (.write "#<LogVar ")
       (.write (str (hash (:log-volatile c))))
       (.write ">"))))

#?(:clj
   (defmethod print-method LogVar2 [c, ^Writer w]
     (doto w
       (.write "#<LogVar2 ")
       (.write (str (hash (:log-volatile c))))
       (.write ">"))))
