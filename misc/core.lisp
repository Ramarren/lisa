
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
(defrule frodo ()
  (frodo (name ?name frodo) (age 0) (has-ring ?ring))
  (bilbo (relative ?name) (age 0))
  (?gandalf (gandalf (name galdalf) (age ?age (= ?age 100))))
  (test (eq ?ring t))
  (not (saruman))
  =>
  (format t "frodo: has-ring is ~S~%" ?ring))

(defrule simple-rule ()
  (frodo (name frodo))
  (bilbo (name bilbo))
  (gandalf (name gandalf) (age 100))
  =>
  (format t "simple-rule fired.~%"))

(defvar *frodo* (assert (frodo (name frodo))))
(defvar *bilbo* (assert (bilbo (name bilbo))))
(defvar *gandalf* (assert (gandalf (name gandalf))))
