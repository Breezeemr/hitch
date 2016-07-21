(ns hitch.eager-go
  (:require [cljs.core.async.impl.ioc-macros :as ioc]))

(defmacro eager-go
  "Asynchronously executes the body, returning immediately to the
  calling thread. Additionally, any visible calls to <!, >! and alt!/alts!
  channel operations within the body will block (if necessary) by
  'parking' the calling thread rather than tying up an OS thread (or
  the only JS thread when in ClojureScript). Upon completion of the
  operation, the body will be resumed.

  Returns a channel which will receive the result of the body when
  completed"
  [& body]
  `(let [c# (cljs.core.async/chan 1)]
     ((fn []
        (let [f# ~(ioc/state-machine body 1 &env ioc/async-custom-terminators)
              state# (-> (f#)
                         (ioc/aset-all! cljs.core.async.impl.ioc-helpers/USER-START-IDX c#))]
          (cljs.core.async.impl.ioc-helpers/run-state-machine-wrapped state#))))
     c#))
