;;; -*- Lisp -*-
;;; File: exercises.lisp
;;; Description: Exercise code to assist in some form of validation
;;; for LISA's grammar.
;;;
;;; $Id: exercises.lisp,v 1.1 2000/10/12 20:23:18 youngde Exp $

(in-package "LISA")

(defrule rule-1
    "Simple rule with no conditions."
  =>
  (halt))

(defrule rule-2
    "Simple rule with no actions."
  (fact-1 x ?x y ?y z 1)
  =>
  (halt))

(defrule rule-3
    "Simple anonymous constraint."
  (fact severity 0)
  =>
  (halt))

(defrule rule-4
    "Simple 'not' connective constraint"
  (fact x (not blue)) ; (fact x ~blue)
  =>
  (halt))

(defrule rule-5
    "More complex 'not' connective constraint"
  (fact (value ? (and (not (red green))))) ; (data-B (value ~red&~green))
  =>
  (halt))

(defrule rule-6
    "Not connective constraint with ordered fact."
  (fact (value ?v (and (not red green))) (key ?k "schtum"))
  =>
  (halt))

(defrule rule-7
    "Rule with 'test' CE."
  (fact-1 ?x)
  (fact-2 (key ?k) (value ?val (not 0)))
  (test (not (= ?x ?val)))
  =>
  (halt))

(defrule rule-8
    "Sample 'or' connective constraint."
  (fact ?x (or (stringp ?x) (symbolp ?x)))
  =>
  (halt))

(defrule rule-9
    (fact (key ?k (or (stringp ?k) (symbolp ?k)))
          (val ?val))
  =>
  (halt))

(defrule rule-10
    "Sample 'or' 'and' connective constraint."
  (fact ?x (or (and (stringp ?x) (string= ?x "foo")) (symbolp ?x)))
  =>
  (halt))
  
(defrule rule-11
    "Sample 'or' 'and' connective constraint."
  (fact (x ?x (or (and (stringp ?x) (string= ?x "foo")) (symbolp ?x)))
        (y ?y))
  =>
  (halt))
  
(defrule rule-12
    "Predicate constraint"
  (fact ?x (numberp ?x))
  =>
  (halt))

(defrule rule-13
    "Predicate constraint using 'not'"
  (fact (x ?x (not (symbolp ?x))))
  =>
  (halt))

(defrule rule-14
    "More predicate constraints."
  (fact-1 ?x (and (numberp ?x) (oddp ?x)))
  (fact-2 (slot ?s (stringp ?s)))
  =>
  (halt))

(defrule rule-15
    "Predicate constraint with salience."
  (declare (salience 100))
  (fact-1 ?y)
  (fact-2 (x ?x (> ?x ?y)) (z 10))
  =>
  (terpri)
  (halt))

(defrule rule-16
    "Return value constraint"
  (fact (x ?x) (y ? (* 2 ?x)))
  =>
  (halt))

(defrule rule-17
    "Assignment pattern."
  ?fact <- (fact x 0 y 1)
  =>
  (retract ?fact))

(defrule rule-18
    "Multiple assignment pattern"
  ?f1 <- (color (not red))
  ?f2 <- (color (not green))
  (test (not (equal ?f1 ?f2)))
  =>
  (halt))



