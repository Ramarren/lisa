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

;;; The basic scenario is a "tank" containing some liquid and a "pump"
;;; attached to the tank. There's a thread associated with the tank that acts
;;; as a consumer of the fluid; there's also a thread running the pump that
;;; restores fluid at an adjustable rate. There are two thresholds (high, low)
;;; at which point the system will attempt to compensate by decreasing or
;;; increasing the pumping rate. This behavior is implemented as a small set
;;; of rules. If the tank runs dry it catches fire; if the tank over-fills it
;;; explodes. As set up now, the simulation tends to fluctuate between both
;;; thresholds; by playing with the variables it's possible to see the tank
;;; destroy itself.

;;; To run the simulation, load this file and (from the LISA-USER package)
;;; evaluate (start-pumps). By default, the only output occurs during
;;; threshold crossings; for more feedback, pass T to the start-pumps
;;; function. To stop the simulation, you must interrupt Lisp then evaluate
;;; (stop-pumps). Alternatively, I suppose one could give (start-pumps) its
;;; own thread; then, interrupting Lisp wouldn't be required. Haven't tried
;;; this, though.

;;; $Id: pumps.lisp,v 1.12 2001/05/10 18:15:49 youngde Exp $

(in-package "LISA-USER")

(defparameter *verbose-output* nil)

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
  `(let ((verbose *verbose-output*))
     (port:make-process
      ,name #'(lambda ()
                (let ((*package* (find-package "LISA-USER"))
                      (*verbose-output* verbose))
                  (progn ,@body))))))

(let ((msg-lock (port:make-lock)))
  (defun message (strm format-string &rest args)
    (port:with-lock (msg-lock)
      (apply #'format strm format-string args)))
  
  (defun debug-message (strm format-string &rest args)
    (when *verbose-output*
      (apply #'message strm format-string args))))
  
(defun set-flow-rate (self new-rate)
  (declare (type pump self))
  (with-accessors ((rate get-flow-rate)) self
    (when (and (not (minusp new-rate)) (not (eql rate new-rate)))
      (debug-message t "Setting new flow rate for pump ~S to ~D.~%"
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
    (incf (get-level self) amount)
    (debug-message t "Increased level in tank ~S by ~D units: new level is ~D.~%"
                   (get-name self) amount (get-level self))
    (mark-instance-as-changed self 'level)))

(defun run-tank (self)
  (declare (type tank self))
  (flet ((adjust-tank-level ()
           (do ()
               ((not (intact-p self)))
             (add-water self (- (random 25)))
             (sleep 0.250))
           (cond ((>= (get-level self) 1000)
                  (message t "Tank ~S exploded!~%" (get-name self)))
                 ((<= (get-level self) 0)
                  (message t "Tank ~S ran dry and caught fire!~%"
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

(defun start-pumps (&optional (verbose nil))
  (let ((*verbose-output* verbose))
    (reset)
    (run)))

(defun stop-pumps ()
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
  (tank (name ?name) (level ?level) (:object ?tank))
  (test (and (low-p ?tank) (intact-p ?tank)))
  (not (tank-level-warning (tank ?tank) (type low)))
  =>
  (assert (tank-level-warning (tank ?tank) (type low)))
  (message t "Warning: Tank ~S is low (~D)!~%" ?name ?level))

(defrule raise-rate-if-low ()
  (?warning (tank-level-warning (tank ?tank) (type low)))
  (pump (name ?name) (tank ?tank) (flow-rate ?flow-rate (< ?flow-rate 25))
        (:object ?pump))
  =>
  (retract ?warning)
  (set-flow-rate ?pump (1+ ?flow-rate))
  (message t "Raised pumping rate of pump ~S to ~D~%" 
           ?name (get-flow-rate ?pump)))

(defrule warn-if-high ()
  (tank (name ?name) (level ?level) (:object ?tank))
  (test (and (high-p ?tank) (intact-p ?tank)))
  (not (tank-level-warning (tank ?tank) (type high)))
  =>
  (assert (tank-level-warning (tank ?tank) (type high)))
  (message t "Warning: Tank ~S is high (~D)!~%" ?name ?level))

(defrule lower-rate-if-high ()
  (?warning (tank-level-warning (tank ?tank) (type high)))
  (pump (name ?name) (flow-rate ?flow-rate (plusp ?flow-rate))
        (:object ?pump))
  =>
  (retract ?warning)
  (let ((new-rate (1- ?flow-rate)))
    (set-flow-rate ?pump new-rate)
    (message t "Lowered pumping rate of pump ~S to ~D~%"
             ?name new-rate)))

(defrule notify-if-ok ()
  (?warning (tank-level-warning))
  (tank (name ?name) (:object ?tank))
  (test (and (not (high-p ?tank)) (not (low-p ?tank))))
  =>
  (retract ?warning)
  (message t "Tank ~S is now OK.~%" ?name))

(defrule sleep-if-bored (:salience -100)
  (?idle (idle (count ?count)))
  =>
  (retract ?idle)
  (sleep 0.250)
  (assert (idle (count (1+ ?count)))))

(defrule report-fire ()
  (tank (name ?name) (:object ?tank))
  (test (and (low-p ?tank) (not (intact-p ?tank))))
  =>
  (message t "*********************************************~%")
  (message t "* Tank ~S has run dry and caught fire.~%" ?name)
  (message t "*********************************************~%")
  (halt (engine)))

(defrule report-explosion ()
  (tank (name ?name) (:object ?tank))
  (test (and (high-p ?tank) (not (intact-p ?tank))))
  =>
  (message t "*********************************************~%")
  (message t "* Tank ~S has overfilled and exploded.~%" ?name)
  (message t "*********************************************~%")
  (halt (engine)))

(defrule startup ()
  =>
  (assert (idle (count 0)))
  (start-run))
