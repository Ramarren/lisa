
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

#+ignore
(defrule simple-rule ()
  (frodo (name frodo))
  (bilbo (name bilbo))
  (gandalf (name gandalf) (age 100))
  =>
  (format t "simple-rule fired.~%"))

(defrule variable-rule ()
  (frodo (name ?name))
  (bilbo (name ?name))
;;;  (bilbo (name ?bname (eq ?name ?bname)))
  =>
  )

(defvar *frodo* (assert (frodo (name frodo))))
(defvar *bilbo* (assert (bilbo (name bilbo))))
(defvar *gandalf* (assert (gandalf (name gandalf) (age 100))))
