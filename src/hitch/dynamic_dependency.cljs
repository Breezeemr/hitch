(ns hitch.dynamic-dependency
  (:require [hitch.protocols :as proto]
            [clojure.set :as set]))

(deftype DependentTransaction [^:mutable current-deps ^:mutable tx-deps ^:mutable in-tx?]
  proto/IDependentTransaction
  (start [_] (set! in-tx? true))
  (add-dep [_ dep]
    (assert in-tx? "must be in transaction to add dependency")
    (set! tx-deps (conj tx-deps dep)))
  (commit [_]
    (let [to-remove (set/difference current-deps tx-deps)]
      (set! current-deps tx-deps)
      (set! tx-deps #{})
      (set! in-tx? false)
      to-remove
      #_(doseq [item to-remove]
        (proto/undepend! )))))


(defn dependent-transaction []
  (->DependentTransaction #{} #{} false))