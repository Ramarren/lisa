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

;;; $Id: pumps.lisp,v 1.3 2001/05/07 15:53:31 youngde Exp $

(in-package "LISA-USER")

(use-engine (make-inference-engine :with-mp t))

(defclass tank ()
  ((level :initarg :level
          :initform 0
          :accessor get-level)
   (name :initarg :name
         :reader get-name)))

(defclass pump ()
  ((flow-rate :initform 0
              :accessor get-flow-rate)
   (tank :initarg :tank
         :reader get-tank)
   (name :initarg :name
         :reader get-name)))

(defun set-flow-rate (self new-rate)
  (declare (type pump self))
  (with-accessors ((rate get-flow-rate)) self
    (when (and (not (minusp new-rate)) (not (eql rate new-rate)))
      (setf rate new-rate)
      (mark-instance-as-changed self 'flow-rate))))

(defun run-pump (self)
  (declare (type pump self))
  (flet ((add-water-to-tank ()
           (with-accessors ((tank get-tank)) self
             (do ()
                 ((not (intact-p tank)) t)
               (add-water tank (get-flow-rate self))
               (lmp:process-sleep 0.10)))))
    (lmp:make-process
     (concatenate 'string "Pump Process " (get-name self))
     #'add-water-to-tank)))

(defun make-pump (name tank)
  (make-instance 'pump :name name :tank tank))

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
    (mark-instance-as-changed self 'level)))

(defun run-tank (self)
  (declare (type tank self))
  (flet ((adjust-tank-level ()
           (do ()
               ((not (intact-p self)))
             (add-water self -1)
             (lmp:process-sleep 0.025))
           (cond ((>= (get-level self) 1000)
                  (format t "Tank ~S exploded!~%" self))
                 ((<= (get-level self) 0)
                  (format t "Tank ~S ran dry and caught fire!~%" self)))))
    (lmp:make-process
     (concatenate 'string "Tank Process " (get-name self))
     #'adjust-tank-level)))

(defimport pump (lisa-user::pump) ())
(defimport tank (lisa-user::tank) ())

(deftemplate tank-level-warning
  (slot tank)
  (slot type))

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

(defrule report-fire ()
  (?fact (tank (name ?name) (:object ?tank)))
  (test (and (low-p ?tank) (not (intact-p ?tank))))
  =>
  (format t "*********************************************~%")
  (format t "* Tank ~S has run dry and caught fire.~%" ?name)
  (format t "*********************************************~%"))

(defrule report-explosion ()
  (?fact (tank (name ?name) (:object ?tank)))
  (test (and (high-p ?tank) (not (intact-p ?tank))))
  =>
  (format t "*********************************************~%")
  (format t "* Tank ~S has overfilled and exploded.~%" ?name)
  (format t "*********************************************~%"))
