(ns hitch.values)

(defrecord Realized [value dependencies])
(defrecord NotRealized [dependencies])



