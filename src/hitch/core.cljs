(ns ^:figwheel-always hitch.core
    (:require [hitch.protocols :as proto]
              [hitch.graph :as store]
              [hitch.fns :as fns]
              [hitch.kv-store ])
  )

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc))

(comment    ;for cursive
  (use 'figwheel-sidecar.repl-api)
  (cljs-repl "dev"))


