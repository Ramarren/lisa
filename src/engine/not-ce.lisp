;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-not-ce)))

(defclass not-ce (conditional-element)
  ((pattern :initarg :pattern
       :reader pattern)))

(defmethod examine ((ce not-ce) &optional (strm t) &key (indent 0))
  (format strm "~VA~S:~%" indent "" (get-class-name ce))
  (examine (pattern ce) strm :indent (+ 2 indent)))

(defun make-not-ce (pattern)
  (make-instance 'not-ce :name "not" :pattern pattern))
