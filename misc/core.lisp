
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
  (slot name)
  (slot friend)
  (slot age))

#+ignore
(defrule frodo ()
  (frodo (name ?name frodo))
  =>
  (format t "frodo fired: ~S~%" ?name))

#+ignore
(defrule not-frodo ()
  (frodo (name ?name (not frodo)))
  =>
  (format t "not-frodo fired: ~S~%" ?name))

#+ignore
(defrule simple-rule ()
  (frodo)
  =>
  (format t "simple-rule fired.~%"))

#+ignore
(defrule special-pattern ()
  ;;;(bilbo (name ?name) (relative ?name))
  (frodo (name ?fname) (has-ring ?ring (eq ?ring ?fname)))
  =>
  )

#+ignore
(defrule negated-slot-rule ()
  (frodo (name (not frodo)))
  =>
  )

#+ignore
(defrule shared-rule-a ()
  (frodo (name frodo))
  (gandalf (name gandalf) (age 100))
  =>
  )

#+ignore
(defrule shared-rule-b ()
  (frodo (name frodo))
  (gandalf (name gandalf) (age 200))
  =>
  )

#+ignore
(defrule constraints ()
  (frodo (name ?name))
  (samwise (name sam) (friend ?friend (not frodo)))
  =>
  (format t "constraints: ~S ~S~%" ?name ?friend))

#+ignore
(defrule variable-rule ()
  (frodo (name ?name))
  (?sam (samwise (name ?name) (friend ?name)))
  =>
  (format t "variable-rule fired: ~S~%" ?sam)
  (modify ?sam (name samwise)))

(defrule or-rule ()
  (frodo (name ?name))
  (or (samwise (name sam) (friend ?name))
      (gandalf (name gandalf)))
  =>
  )

#+ignore
(defrule samwise ()
  (samwise (name samwise))
  =>
  (format t "Rule samwise fired.~%"))

#+ignore
(defrule test-rule ()
  (frodo (name ?name))
  (samwise (friend ?name) (age ?age))
  (test (eq ?age 100))
  =>
  )

#+ignore
(defrule negated-variable ()
  (frodo (name ?name))
  (samwise (friend (not ?name)))
  =>
  )

#+ignore
(defrule simple ()
  (?f (gandalf (age 100)))
  =>
  (let ((?age 1000))
    (modify ?f (age ?age) (name (intern (make-symbol "gandalf"))))))

#+ignore
(defrule embedded-rule ()
  (gandalf (name gandalf) (age ?age))
  =>
  (defrule new-gandalf ()
    (gandalf (name new-gandalf) (age ?age))
    =>
    (format t "new-gandalf fired.~%")))

#|
(defparameter *frodo* (assert (frodo (name frodo))))
(defparameter *bilbo* (assert (bilbo (name bilbo))))
(defparameter *samwise* (assert (samwise (friend frodo) (age 100))))
(defparameter *gandalf* (assert (gandalf (name gandalf) (age 200))))
|#
