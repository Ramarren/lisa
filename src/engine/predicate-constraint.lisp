;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-predicate-constraint
            examine)))

(defclass predicate-constraint (constraint)
  ((function :initarg :function
             :reader function-call)))

(defmethod examine ((pc predicate-constraint) &optional (strm t) &key (indent 0))
  (format strm "~VA~S:~%" indent "" (get-class-name pc))
  (examine (function-call pc) strm :indent (+ 2 indent)))

(defun make-predicate-constraint (func)
  (make-instance 'predicate-constraint
    :function func))
