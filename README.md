# Hitch

A Clojurescript library designed to manage and cache derived data.

## Usage
[hitch "0.1.0-SNAPSHOT"]


Hitch allows the user to define selectors which are functions that can depend on other selectors
(def-selector symbol [constructor-binding-forms] [let-selector-forms] 
body)
(def-selector name [args ...] )



(hitch target data-selector)

(hitch-apply target fn arg1 arg2 & rest)

# Uniqueness

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
