;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-pattern-ce)))

(defclass pattern-ce (conditional-element)
  ((name :initarg :name
         :reader name)
   (pattern-list :initarg :pattern-list
                 :reader pattern-list)))

(defun make-pattern-ce (name patterns)
  (make-instance 'pattern-ce :name name :pattern-list patterns))
