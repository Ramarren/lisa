;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: pumps.lisp
;;; Description: The "Tanks and Pumps" application, as adapted from the Java
;;; Expert System Shell (Jess). This is a pretty good test of LISA's MP
;;; support.

;;; $Id: pumps.lisp,v 1.8 2001/05/09 20:12:56 youngde Exp $

(in-package "LISA-USER")

(use-default-engine)

(defclass tank ()
  ((level :initarg :level
          :initform 500
          :accessor get-level)
   (lock :initform (port:make-lock :name "Tank Lock")
         :reader get-lock)
   (name :initarg :name
         :reader get-name)))

(defclass pump ()
  ((flow-rate :initform 0
              :accessor get-flow-rate)
   (tank :initarg :tank
         :reader get-tank)
   (lock :initform (port:make-lock :name "Pump Lock")
         :reader get-lock)
   (name :initarg :name
         :reader get-name)))

(defmacro with-new-process ((name) &body body)
  `(port:make-process
    ,name #'(lambda ()
              (let ((*package* (find-package "LISA-USER")))
                (progn ,@body)))))

(defun set-flow-rate (self new-rate)
  (declare (type pump self))
  (with-accessors ((rate get-flow-rate)) self
    (when (and (not (minusp new-rate)) (not (eql rate new-rate)))
      (format t "Setting new flow rate for pump ~S to ~D.~%"
              (get-name self) new-rate)
      (setf rate new-rate)
      (mark-instance-as-changed self 'flow-rate))))

(defun run-pump (self)
  (declare (type pump self))
  (flet ((add-water-to-tank ()
           (with-accessors ((tank get-tank)) self
             (do ()
                 ((not (intact-p tank)) t)
               (add-water tank (get-flow-rate self))
               (sleep 0.10)))))
    (with-new-process ((concatenate 'string "Pump:" (get-name self)))
      (add-water-to-tank))))

(defun make-pump (name tank)
  (make-instance 'pump :name name :tank tank))

(defmethod get-level :around ((self tank))
  (port:with-lock ((get-lock self))
    (call-next-method self)))

(defmethod (setf get-level) :around (new-value (self tank))
  (port:with-lock ((get-lock self))
    (call-next-method new-value self)))

(defun high-p (self)
  (declare (type tank self))
  (> (get-level self) 750))

(defun low-p (self)
  (declare (type tank self))
  (< (get-level self) 250))

(defun intact-p (self)
  (declare (type tank self))
  (with-accessors ((level get-level)) self
    (and (< level 1000) (> level 0))))

(defun add-water (self amount)
  (declare (type tank self) (type integer amount))
  (unless (zerop amount)
    (format t "Increasing level in tank ~S by ~D units.~%"
            (get-name self) amount)
    (incf (get-level self) amount)
    (mark-instance-as-changed self 'level)))

(defun run-tank (self)
  (declare (type tank self))
  (flet ((adjust-tank-level ()
           (do ()
               ((not (intact-p self)))
             (add-water self -10)
             (sleep 0.250))
           (cond ((>= (get-level self) 1000)
                  (format t "Tank ~S exploded!~%" (get-name self)))
                 ((<= (get-level self) 0)
                  (format t "Tank ~S ran dry and caught fire!~%"
                          (get-name self))))))
    (with-new-process ((concatenate 'string "Tank:" (get-name self)))
      (adjust-tank-level))))

(defun make-tank (name)
  (make-instance 'tank :name name))

(defvar *equipment* '())

(defun start-run ()
  (let* ((tank (make-tank "TankMain"))
         (pump (make-pump "PumpMain" tank)))
    (setf *equipment* nil)
    (push (cons tank (run-tank tank)) *equipment*)
    (push (cons pump (run-pump pump)) *equipment*)
    (assert-instance tank)
    (assert-instance pump)
    (values *equipment*)))

(defun stop-run ()
  (halt (current-engine))
  (mapc #'(lambda (equip)
            (port:kill-process (rest equip)))
        *equipment*)
  (values nil))

(defimport pump (lisa-user::pump) ())
(defimport tank (lisa-user::tank) ())

(deftemplate tank-level-warning ()
  (slot tank)
  (slot type))

(deftemplate idle ()
  (slot count))

(defrule warn-if-low ()
  (tank (name ?name) (:object ?tank))
  (test (and (low-p ?tank) (intact-p ?tank)))
  (not (tank-level-warning (tank ?tank) (type low)))
  =>
  (assert (tank-level-warning (tank ?tank) (type low)))
  (format t "Warning: Tank ~S is low!~%" ?name))

(defrule raise-rate-if-low ()
  (?warning (tank-level-warning (tank ?tank)))
  (pump (name ?name) (tank ?tank) (flow-rate ?flow-rate (< ?flow-rate 25))
        (:object ?pump))
  =>
  (retract ?warning)
  (set-flow-rate ?pump (1+ ?flow-rate))
  (format t "Raised pumping rate of pump ~S to ~D~%" 
          ?name (get-flow-rate ?pump)))

(defrule warn-if-high ()
  (tank (name ?name) (:object ?tank))
  (test (and (high-p ?tank) (intact-p ?tank)))
  (not (tank-level-warning (tank ?tank) (type high)))
  =>
  (assert (tank-level-warning (tank ?tank) (type high)))
  (format t "Warning: Tank ~S is high!~%" ?name))

(defrule lower-rate-if-high ()
  (?warning (tank-level-warning (tank ?tank) (type high)))
  (pump (name ?name) (flow-rate ?flow-rate (plusp ?flow-rate))
        (:object ?pump))
  =>
  (retract ?warning)
  (set-flow-rate ?pump (1- ?flow-rate))
  (format t "Lowered pumping rate of pump ~S to ~D~%"
          ?name (get-flow-rate ?pump)))

(defrule notify-if-ok ()
  (?warning (tank-level-warning))
  (tank (name ?name) (:object ?tank))
  (test (and (not (high-p ?tank)) (not (low-p ?tank))))
  =>
  (retract ?warning)
  (format t "Tank ~S is now OK.~%" ?name))

(defrule sleep-if-bored (:salience -100)
  (?idle (idle (count ?count)))
  =>
  (retract ?idle)
  (sleep 0.250)
  (assert (idle (count (1+ ?count)))))

(defrule report-fire ()
  (?fact (tank (name ?name) (:object ?tank)))
  (test (and (low-p ?tank) (not (intact-p ?tank))))
  =>
  (format t "*********************************************~%")
  (format t "* Tank ~S has run dry and caught fire.~%" ?name)
  (format t "*********************************************~%")
  (retract ?fact)
  (halt (engine)))

(defrule report-explosion ()
  (?fact (tank (name ?name) (:object ?tank)))
  (test (and (high-p ?tank) (not (intact-p ?tank))))
  =>
  (format t "*********************************************~%")
  (format t "* Tank ~S has overfilled and exploded.~%" ?name)
  (format t "*********************************************~%")
  (retract ?fact)
  (halt (engine)))

(defrule startup ()
  =>
  (assert (idle (count 0)))
  (start-run))
