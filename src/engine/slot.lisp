;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-slot
            examine)))

(defclass slot ()
  ((name :initarg :name
         :reader name)
   (value :initarg :value
          :reader value)
   (tests :initarg :tests
          :initform nil
          :reader tests)))

(defmethod examine ((s slot) &optional (strm t) &key (indent 0))
  (let ((pad (+ 2 indent)))
    (format strm "~VA~S:~%" indent "" (get-class-name s))
    (format strm "~VAslot: (~S ~S)~%" pad "" (name s) (value s))
    (dolist (ent (tests s))
      (format strm "~VAconstraint: " pad "")
      (examine ent strm))))
  
(defun make-slot (name value &optional tests)
  (make-instance 'slot :name name :value value
                 :tests tests))
