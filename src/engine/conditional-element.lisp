;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(name)))

(defclass conditional-element ()
  ((name :initarg :name
        :reader name)))
