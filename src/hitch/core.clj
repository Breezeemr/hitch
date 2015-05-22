(ns hitch.core)

(defmacro if-loaded [bindingval loaded not-loaded]
  `(let ~bindingval
     (if (= ~(first bindingval) :hitch/not-loaded)
       ~not-loaded
       ~loaded)))