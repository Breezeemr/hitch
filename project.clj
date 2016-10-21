(defproject com.breezeehr/hitch "0.1.6-SNAPSHOT"
  :description "A Clojurescript library designed to manage and cache derived data."
  :url "https://github.com/Breezeemr/hitch"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :scm "https://github.com/Breezeemr/hitch"

  :source-paths ["src"]

  :profiles
  {:provided
   {:dependencies [[org.clojure/clojure "1.8.0"]
                   [org.clojure/clojurescript "1.9.93"]]}

   :dev
   {:dependencies   [[com.cemerick/piggieback "0.2.1"]      ; needed by figwheel nrepl
                     [devcards "0.2.1"]]
    :plugins        [[lein-figwheel "0.5.4-7"]]
    :figwheel       {:http-server-root "public"
                     :nrepl-port       7889
                     :server-logfile   "target/figwheel-logfile.log"}
    :resource-paths ["dev-resources" "target/devcards"]
    :cljsbuild      {:builds
                     [{:id           "devcards"
                       :source-paths ["src" "test"]
                       :figwheel     {:devcards true}
                       :compiler     {:main                 hitch.test-runner
                                      :asset-path           "js/out"
                                      :output-to            "target/devcards/public/js/hitch_devcards.js"
                                      :output-dir           "target/devcards/public/js/out"
                                      :optimizations        :none
                                      :source-map           true
                                      :source-map-timestamp false
                                      :cache-analysis       true}}]}}})
