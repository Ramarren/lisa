;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: slot.lisp
;;; Description: Represents a single slot within a pattern.

;;; $Id: slot.lisp,v 1.14 2001/01/30 22:18:44 youngde Exp $

(in-package :lisa)

(defclass slot ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (value :initarg :value
          :accessor get-value)
   (constraint :initarg :constraint
               :initform nil
               :accessor get-constraint)
   (locality :initform t
             :reader get-locality))
  (:documentation
   "Represents a single slot within a pattern."))

(defun is-literal-slotp (self)
  (declare (type slot self))
  (literalp (get-value self)))

(defun has-constraintp (slot)
  (declare (type slot slot))
  (not (null (get-constraint slot))))

(defun localized-slotp (slot)
  (get-locality slot))

(defun slot-has-global-binding (slot)
  (setf (slot-value slot 'locality) nil))

(defmethod print-object ((self slot) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; value = ~S ; constraint = ~S)"
            (get-name self) (get-value self)
            (get-constraint self))))

(defun make-slot (name value constraint)
  (make-instance 'slot :name name :value value :constraint constraint))

(defclass optimisable-slot (slot)
  ()
  (:documentation
   "A subclass of SLOT describing a slot instance that's eligible for
   certain optimisations."))

(defclass optimisable-negated-slot (optimisable-slot)
  ()
  (:documentation
   "A subclass of SLOT describing a slot instance that's eligible for
   certain optimisations, AND is negated."))

(defmethod get-value ((self optimisable-negated-slot))
  (second (slot-value self 'value)))
