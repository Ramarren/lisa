;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-function-call
            examine)))

(defclass function-call ()
  ((name :initarg :name
         :reader name)
   (arguments :initarg :arguments
              :reader arguments)))

(defmethod examine ((func function-call) &optional (strm t) &key (indent 0))
  (let ((pad (+ 2 indent)))
    (format strm "~VA~S:~%" indent "" (get-class-name func))
    (format strm "~VAname: ~S~%" pad "" (name func))
    (format strm "~VAarguments: ~S~%" pad "" (arguments func))))

(defun make-function-call (func args)
  (make-instance 'function-call :name func
                 :arguments args))
