
(in-package "LISA")

(use-default-engine)

#+ignore
(deftemplate frodo ()
  (slot name (default frodo))
  (slot has-ring (default nil))
  (slot age (default 0)))

(deftemplate frodo ()
  (slot name)
  (slot has-ring)
  (slot age (default 0)))

(deftemplate bilbo ()
  (slot name (default bilbo))
  (slot relative)
  (slot age (default 0)))

(deftemplate gandalf ()
  (slot name (default gandalf))
  (slot age (default 0)))

(deftemplate saruman ()
  (slot name (default saruman)))

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
