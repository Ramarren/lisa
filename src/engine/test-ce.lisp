;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-test-ce)))

(defclass test-ce (conditional-element)
  ((test-function :initarg :test-function
                  :reader test-function)))

(defmethod examine ((ce test-ce) &optional (strm t) &key (indent 0))
  (format strm "~VA~S:~%" indent "" (get-class-name ce))
  (examine (test-function ce) strm :indent (+ 2 indent)))

(defun make-test-ce (func)
  (make-instance 'test-ce :name "test" :test-function func))
