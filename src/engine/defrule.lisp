;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-defrule
            add-pattern
            add-action
            examine)))

(defclass defrule ()
  ((name :initarg :name
         :initform nil
         :reader name)
   (comment :initform nil
            :initarg :comment
            :accessor comment)
   (salience :initform 0
             :initarg :salience
             :accessor salience)
   (patterns :initform (make-array 1 :adjustable t
                                   :fill-pointer 0)
             :reader patterns)
   (actions :initform (make-array 1 :adjustable t
                                  :fill-pointer 0)
            :reader actions)))

(defmethod add-pattern ((rule defrule) pattern)
  (vector-push-extend pattern (patterns rule)))

(defmethod add-action ((rule defrule) action)
  (vector-push-extend action (actions rule)))

(defmethod examine ((rule defrule) &optional (strm t) &key (indent 0))
  (format strm "~S:~%" (get-class-name rule))
  (format strm "  name: ~S~%" (name rule))
  (format strm "  comment: ~S~%" (comment rule))
  (format strm "  salience: ~A~%" (salience rule))
  (examine-patterns rule strm)
  (examine-actions rule strm))

(defmethod examine-patterns ((rule defrule) strm)
  (format strm "  patterns:~%")
  (with-accessors ((patterns patterns)) rule
    (dotimes (i (length patterns) t)
      (examine (aref patterns i) strm :indent 4))))

(defmethod examine-actions ((rule defrule) strm)
  (format strm "  actions: ~%")
  (with-accessors ((actions actions)) rule
    (dotimes (i (length actions) t)
      (examine (aref actions i) strm :indent 4))))

;;
;; Constructor for class 'defrule'...
;;

(defun make-defrule (name &key comment (salience 0))
  (make-instance 'defrule :name name :comment comment
                 :salience salience))

