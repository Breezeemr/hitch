# Hitch

A Clojurescript library designed to manage and cache derived data.

## Usage
[hitch "0.1.0-SNAPSHOT"]

## Why?
Hitch allows you to define selectors which provide the user with a lazy map or store. It tracks the dependencies
and streamlines concerns like garbage collection, loading and initialization.

a function takes inputs that are immediately available in memory and returns an immediately returns output

a selector is a construct that attempts to run a function repeatedly until it can finish completely. The function for a
Selector takes a graph and the inputs required to build other selector. Finishing completely means that all parent selectors which a
selector function requires are available.




Hitch is a database allows the user to define selectors which are functions that can depend on other selectors
(def-selector symbol [constructor-binding-forms] [let-selector-forms] 
body)
(def-selector name [args ...] )






(hitch target data-selector)

(hitch-apply target fn arg1 arg2 & rest)

# Uniqueness

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
