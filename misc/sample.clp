(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(deftemplate natasha
  (slot name)
  (slot nemesis))

(load-package jess.ViewFunctions)

(defrule rocky-1
  (rocky (name ?name&:(eq ?name "rocky")))
  (boris (name ?bname&:(eq ?bname ?name)))
  =>
  (format t "rocky-1 fired: ?name = ~S~%" ?name))

(defrule rocky-2
  (rocky (name ?name&:(eq ?name "rocky")))
  (boris (name ?cname&:(eq ?cname ?name)))
  =>
  (format t "rocky-2 fired: ?name = ~S~%" ?name))
