
(in-package :lisa-user)

(defclass frodo ()
  ((name :initarg :name
         :reader name)))

(defclass has-ring () ())

(defrule frodo ()
  (frodo (name frodo))
  =>
  (assert (has-ring) :cf 0.9))