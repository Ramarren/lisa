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

;;; $Id: pumps.lisp,v 1.1 2001/05/05 21:00:52 youngde Exp $

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

(defimport pump (lisa-user::pump) ())
(defimport tank (lisa-user::tank) ())

(defun set-flow-rate (self new-rate)
  (declare (type pump self))
  (with-accessors ((rate get-flow-rate)) self
    (when (and (>= new-rate 0) (not (= rate new-rate)))
      (setf rate new-rate)
      (mark-instance-as-changed self 'flow-rate))))

(defun run-pump (self)
  (declare (type pump self)))

(defun make-pump (name tank)
  (make-instance 'pump :name name :tank tank))
