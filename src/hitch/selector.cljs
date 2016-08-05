(ns hitch.selector
  (:require-macros [hitch.eager :refer [go]])
  (:require [hitch.protocols :as proto]
            [cljs.core.async.impl.channels :as impl-chan]
            [cljs.core.async.impl.protocols :as impl]
            [hitch.nodes.simple]
            [cljs.core.async :as async]))
