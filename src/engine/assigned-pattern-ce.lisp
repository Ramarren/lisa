;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-assigned-pattern-ce
            varname
            pattern)))

(defclass assigned-pattern-ce (conditional-element)
   ((pattern :initarg :pattern
             :reader pattern)))

(defmethod examine ((ce assigned-pattern-ce) &optional (strm t) &key (indent 0))
  (let ((pad (+ 2 indent)))
    (format strm "~VA~S:~%" indent "" (get-class-name ce))
    (format strm "~VAvariable: ~S~%" pad "" (name ce))
    (format strm "~VApattern:~%" pad "")
    (examine (pattern ce) strm :indent (+ 2 pad))))

(defun make-assigned-pattern-ce (varname pattern)
  (make-instance 'assigned-pattern-ce :name varname :pattern pattern))

