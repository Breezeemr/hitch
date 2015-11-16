(ns hitch.dynamic-dependency)

(defn- wdep [dependent body]
  `(do
     (binding [hitch.protocols/*dependent* ~dependent]
         (hitch.protocols/start (.-refs ~dependent))
         (let [temp# (do ~@body)]
           #_(doseq [to-remove# (hitch.protocols/commit (.-refs ~dependent))]
             (hitch.protocols/undepend! to-remove# ~dependent))
           temp#)
         )))

(defmacro with-dependent [dependent & body]
  (wdep dependent body))
