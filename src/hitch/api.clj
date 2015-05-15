(ns hitch.api)

(defmacro if-loaded [bindingval loaded not-loaded]
  `(let ~bindingval
     (if (= ~(first bindingval) :hitch.api/not-loaded)
       ~not-loaded
       ~loaded)))