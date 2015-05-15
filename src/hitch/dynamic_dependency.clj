(ns hitch.dynamic-dependency)

(defn- wdep [dependent body]
  `(do
     (binding [hitch.protocols/*dependent* ~dependent]
         (hitch.protocols/start (.-refs ~dependent))
         (let [temp# (do ~@body)]
           (doseq [to-remove# (hitch.protocols/commit (.-refs ~dependent))]
             (hitch.protocols/un-depend! to-remove# ~dependent))
           temp#)
         )))

(defmacro with-dependent [dependent & body]
  (wdep dependent body))
