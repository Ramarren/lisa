;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(defclass salience-ce (conditional-element)
  ((salience :initarg salience
             :accessor salience)))

(defun make-salience-ce (salience)
  (make-instance 'salience-ce :salience salience))
