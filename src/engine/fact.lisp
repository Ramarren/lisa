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

;;; File: fact.lisp
;;; Description: Represents facts in the knowledge base.

;;; $Id: fact.lisp,v 1.5 2000/11/17 16:34:45 youngde Exp $

(in-package :lisa)

(defclass fact ()
  ((class :initarg :class
         :initform nil
         :reader get-class)
   (fact-id :initarg :fact-id
            :initform nil
            :accessor get-fact-id)
   (slot-table :initform (make-hash-table)
               :accessor get-slot-table)
   (clock :initarg :clock
          :initform 0
          :accessor get-clock))
  (:documentation
   "Represents facts in the knowledge base."))

(defmethod get-name ((self fact))
  (class-name (get-class self)))

(defmethod set-slot-value ((self fact) slot value)
  (setf (gethash slot (get-slot-table self)) value))

(defmethod get-slot-value ((self fact) slot)
  (gethash slot (get-slot-table self)))
  
(defmethod get-time ((self fact))
  (get-clock self))

(defmethod update-time ((self fact) (engine rete))
  (setf (get-clock self) (get-time engine)))

(defmethod equals ((self fact) (obj fact))
  (equal (get-class self) (get-class obj)))

(defmethod initialize-instance :after ((self fact) &key (slots nil))
  (let ((map (slot-value self 'slot-table)))
    (mapc #'(lambda (slot)
              (setf (gethash (first slot) map) (second slot)))
          slots)))

(defun make-fact (class slots)
  (make-instance 'fact :class class :slots slots))
