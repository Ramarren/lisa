(deftemplate rocky
  (slot name))

(deftemplate boris
  (slot name))

(deftemplate natasha
  (slot name)
  (slot nemesis))

(load-package jess.ViewFunctions)
(set-node-index-hash 1)

(defrule rocky-1
  (rocky (name ?name&:(eq ?name "rocky")))
  (boris (name ?boris&:(eq ?boris ?name)))
  =>
  (format t "rocky-1 fired: ?name = ~S~%" ?name))

(defrule rocky-2
  (rocky (name ?name&:(eq ?name "rocky")))
  (boris (name ?schtum&:(eq ?schtum ?name)))
  =>
  (format t "rocky-2 fired: ?name = ~S~%" ?name))
