(in-package :lisa)

(use-default-engine)
(clear)

(defmacro define-lisa-class (name supers &body body)
  (let* ((slot-specs (first body))
         (exports (loop for sd in slot-specs collect (car sd))))
    `(progn
       (export '(,name ,@exports))
       (defclass ,name ,supers ,@body)
       (defimport ,name))))

(defclass mtime ()
  ((secs :initform 0 :initarg :secs)
   (usec :initform 0 :initarg :usec))
  (:documentation "Time relative to some TBD base."))

(defun make-mtime (&optional (s 0)(u 0))
  (make-instance 'mtime :secs s :usec u))

(defun make-mtimef (float-num)
  (multiple-value-bind (ip fp)(truncate float-num)
    (make-mtime ip (truncate (* fp 1e6)))))

(defmacro with-mtime-as-float ((var mtime) &body body)
  `(with-slots (secs usec) ,mtime
     (let ((,var (+ (coerce secs 'double-float)
                    (* (coerce usec 'double-float) 1d-6))))
       ,@body)))

(defmethod print-object ((o mtime) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (with-mtime-as-float (v o)
      (format stream "~,6f" v))))

(defmethod mtime< ((t1 mtime)(t2 mtime))
  (with-slots ((s1 secs)(u1 usec)) t1
    (with-slots ((s2 secs)(u2 usec)) t2
      (format t "s1 is ~S, u1 is ~S~%" s1 u1)
      (format t "s2 is ~S, u2 is ~S~%" s2 u2)
      (if (= s1 s2)
          (< u1 u2)
        (< s1 s2)))))

(defvar *system-clock*)

(define-lisa-class system-clock ()
  ((time :initform (make-mtime) :initarg :time)))

(define-lisa-class timed-fact ()
  ((start-time   :initform (make-mtime) :initarg :start-time)
   (mission-time :initform (make-mtime))))

(defrule update-timed-fact ()
  (system-clock (time ?system-time))
  (?tf (timed-fact (mission-time ?mtime (mtime< ?mtime ?system-time))
                   (:object ?timed-fact)))
  =>
  (format t "timed-fact-instance is: ~S~%" ?timed-fact)
  (format t "system time is: ~S~%" ?system-time)
  (format t "mission-time before mod: ~S~%"
          (slot-value ?timed-fact 'mission-time))
  (modify ?tf (mission-time ?system-time))
  (format t "mission-time after mod: ~S~%"
          (slot-value ?timed-fact 'mission-time)))

(define-lisa-class id-mixin ()
  ((id :initarg :index :reader id-of :initform 0)))

(define-lisa-class tx-Ping (timed-fact id-mixin)
  ((start   :initarg :start :initform nil)
   (status  :initarg :status :initform nil)
   (token   :initarg :token :initform 0)
   (rx-beam :initarg :rx-beam :initform 0)
   (aimed   :initarg :direction :initform 0.0)
   (vsa     :initarg :vsa :initform 0.0))
  (:documentation
   "Represent ensonification due to an active transmission"))

(defrule init-other ()
  =>
  (setq *system-clock* (make-instance 'system-clock))
  (assert-instance *system-clock*)
  )

(defun fooey ()
  (assert-instance (make-instance 'tx-ping ))
  (setf (slot-value *system-clock* 'time)(make-mtimef 10.0))
  (mark-instance-as-changed *system-clock* :slot-id 'time)
  (watch :activations))

(reset)
(run)
(fooey)
(walk)

;;; Now, why is UPDATE-TIMED-FACT always on the agenda even though
;;; (FACTS) shows that it shouldn't be? Note that if ID-MIXIN is removed
;;; from the TX-PING class definition things seem to work correctly?
