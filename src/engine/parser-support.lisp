;; -*- Lisp -*-

(in-package "LESS.PARSER")

(defun build-defrule (name comment declarations lhs rhs)
  (let ((rule (le:make-defrule name :comment comment)))
    (dolist (ce lhs)
      (le:add-pattern rule ce))
    (dolist (action rhs)
      (le:add-action rule action))
    rule))

(defun build-unordered-pattern (name &key (slots nil))
  (let ((pattern (le:make-unordered-pattern-ce name)))
    (dolist (ent slots)
      (let ((slot-name (first ent))
            (slot-value (second ent))
            (slot-tests (third ent)))
        (le:add-slot pattern
                     (le:make-slot slot-name slot-value
                                   (if (listp slot-tests)
                                       slot-tests
                                     (list slot-tests))))))
    pattern))

(defun build-ordered-pattern (name constraints)
  (le:make-ordered-pattern-ce name constraints))

(defun build-assigned-pattern (varname pattern)
  (le:make-assigned-pattern-ce varname pattern))

(defun build-test-pattern (func)
  (le:make-test-ce (build-function func)))

(defun build-not-pattern (ce)
  (le:make-not-ce ce))

(defun build-predicate-constraint (func)
  (le:make-predicate-constraint
   (build-function func)))

(defun build-return-value-constraint (func)
  (le:make-return-value-constraint
   (build-function func)))

(defun build-action (func)
  (build-function func))

(defun build-function (func)
  (le:make-function-call (first func) (rest func)))
