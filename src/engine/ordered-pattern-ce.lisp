;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-ordered-pattern-ce)))

(defclass ordered-pattern-ce (conditional-element)
  ((field-list :initarg :field-list
               :reader field-list)))

(defmethod examine ((ce ordered-pattern-ce) &optional (strm t) &key (indent 0))
  (let ((pad (+ 2 indent)))
    (format strm "~VA~S:~%" indent "" (get-class-name ce))
    (format strm "~VAname: ~S:~%" pad "" (name ce))
    (format strm "~VAfields: ~S~%" pad "" (field-list ce))))

(defun make-ordered-pattern-ce (name fields)
  (make-instance 'ordered-pattern-ce :name name :field-list fields))

