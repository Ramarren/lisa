
(in-package "LISA")

(use-default-engine)

(deftemplate frodo ()
  (slot name)
  (slot has-ring)
  (slot age))

(deftemplate bilbo ()
  (slot name)
  (slot relative)
  (slot age))

(deftemplate gandalf ()
  (slot name)
  (slot age))

(deftemplate saruman ()
  (slot name))

(deftemplate samwise ()
  (slot friend))

#+ignore
(defrule simple-rule ()
  (frodo (name frodo))
  (bilbo (name bilbo))
  (gandalf (name gandalf) (age 100))
  =>
  (format t "simple-rule fired.~%"))

(defrule variable-rule ()
  (frodo (name ?name))
  (samwise (friend ?name))
  =>
  )

(defvar *frodo* (assert (frodo (name frodo))))
(defvar *bilbo* (assert (bilbo (name bilbo))))
(defvar *samwise* (assert (samwise (friend frodo))))
(defvar *gandalf* (assert (gandalf (name gandalf) (age 100))))
