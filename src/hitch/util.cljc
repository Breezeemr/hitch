(ns hitch.util)

;; Is a function in cljs because js vms deopt functions which throw
#?(:cljs
   (defn throw-ex-info
     ([msg data] (throw (ex-info msg data)))
     ([msg data cause] (throw (ex-info msg data cause))))
   :default
   (defmacro throw-ex-info
     ([msg data] `(throw (ex-info ~msg ~data)))
     ([msg data cause] `(throw (ex-info ~msg ~data ~cause)))))

(defn- rsome
  "A `some` which uses reduction."
  [pred coll]
  (reduce (fn [_ x]
            (when-let [r (pred x)]
              (reduced r)))
    nil coll))

(defonce UNKNOWN
  (reify
    #?@(:clj  [Object
               (toString [_] "#<UNKNOWN>")]
        :cljs [IPrintWithWriter
               (-pr-writer [_ writer opts]
                 (-write writer "#<UNKNOWN>"))])))

(defn- unknown? [x] (identical? UNKNOWN x))
