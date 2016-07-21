(ns ^:figwheel-always hitch.test-runner
  (:require [hitch.core-test]
            [hitch.selectors.mutable-var-test]
            [hitch.selectors.kv-store-test]
            [cljs.test :as test :include-macros true :refer [report]])
  (:import [goog.dom DomHelper]
           [goog dom]))

(enable-console-print!)

(defn clear-results []
  (.setTextContent dom (.getElement dom "results") "")
  )

(defn append-results [item]
  (.append dom (.getElement dom "results") item (.createDom dom "br")))

(defn color-favicon-data-url [color]
  (let [cvs (.createElement js/document "canvas")]
    (set! (.-width cvs) 16)
    (set! (.-height cvs) 16)
    (let [ctx (.getContext cvs "2d")]
      (set! (.-fillStyle ctx) color)
      (.fillRect ctx 0 0 16 16))
    (.toDataURL cvs)))

(defn change-favicon-to-color [color]
  (let [icon (.getElementById js/document "favicon")]
    (set! (.-href icon) (color-favicon-data-url color))))

(defmethod report [::test/default :summary] [m]
  (append-results (pr-str m))
  (println "\nRan" (:test m) "tests containing"
           (+ (:pass m) (:fail m) (:error m)) "assertions.")
  (println (:fail m) "failures," (:error m) "errors.")
  (if (< 0 (+ (:fail m) (:error m)))
    (change-favicon-to-color "#d00")
    (change-favicon-to-color "#0d0")))                      ;;<<-- change color

(defmethod report [::test/default :fail] [m]
  (append-results (pr-str m))
  (test/inc-report-counter! :fail))

(defmethod report [::test/default :error] [m]
  (append-results (pr-str m))
  (test/inc-report-counter! :error))

(defn runner []
  (clear-results)
  (test/run-tests
    'hitch.core-test
    'hitch.selectors.mutable-var-test
    'hitch.selectors.kv-store-test))

(defn ^:export on-js-reload []
  (runner))

(defn ^:export run []
  (runner))

(comment                                                    ;for cursive
  (use 'figwheel-sidecar.repl-api)
  (cljs-repl "test"))