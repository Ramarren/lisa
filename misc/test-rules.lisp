
(in-package "LISA-USER")

(use-default-engine)

(defclass rocky ()
  ((name :initarg :name)
   (buddy :initarg :buddy)))

(defclass boris ()
  ((name :initarg :name)
   (comrade :initarg :comrade)))

(defimport rocky)

(defimport boris)

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
  (format t "natasha fired! Instance is ~S~%" ?obj)
  (format t "engine is ~S~%" (engine)))
