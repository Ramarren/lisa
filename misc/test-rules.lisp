
(in-package "LISA-USER")

(defclass rocky ()
  ((name :initarg :name)
   (buddy :initarg :buddy)))

(defclass boris ()
  ((name :initarg :name)
   (comrade :initarg :comrade)))

(defimport rocky (lisa-user::rocky)
  (name buddy))

(defimport boris (lisa-user::boris)
  (name comrade))

(defparameter *rocky*
  (make-instance 'rocky :name "rocky" :buddy "bullwinkle"))
(defparameter *boris* nil)

(defrule rocky ()
  (rocky (name "rocky") (buddy ?buddy))
  =>
  (format t "rocky fired!~%")
  (setf *boris* (make-instance 'boris :name "boris" :comrade ?buddy))
  (assert-instance *boris*))

(defrule boris ()
  (?boris (boris (name "boris")))
  =>
  (format t "boris fired!~%")
  (modify ?boris (name "natasha")))

(defrule natasha ()
  (boris (name "natasha") (:object ?obj))
  =>
  (format t "natasha fired! Instance is ~S~%" ?obj))

(defmethod tell-lisa-modified-instance ((self boris) slot)
  (format t "LISA modified slot ~S of boris instance ~S~%" slot self))
