;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-return-value-constraint
            examine)))

(defclass return-value-constraint (constraint)
  ((function :initarg :function
             :reader function-call)))

(defmethod examine ((pc return-value-constraint) &optional (strm t) &key (indent 0))
  (format strm "~VA~S:~%" indent "" (get-class-name pc))
  (examine (function-call pc) strm :indent (+ 2 indent)))

(defun make-return-value-constraint (function)
  (make-instance 'return-value-constraint :function function))
