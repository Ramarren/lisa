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
;;; Description: This class represents facts in the knowledge base.

;;; $Id: fact.lisp,v 1.16 2001/01/27 00:26:58 youngde Exp $

(in-package :lisa)

(defclass fact ()
  ((class :initarg :class
         :initform nil
         :reader get-class)
   (fact-id :initarg :fact-id
            :initform -1
            :reader get-fact-id)
   (symbolic-id :reader get-symbolic-id)
   (slot-source :initarg :slot-source
                :reader get-slot-source)
   (slot-table :initform nil
               :accessor get-slot-table)
   (clock :initarg :clock
          :initform 0
          :accessor get-clock))
  (:documentation
   "This class represents facts in the knowledge base."))

(defmethod get-name ((self fact))
  (class-name (get-class self)))

(defmethod set-fact-id ((self fact) id)
  (setf (slot-value self 'fact-id) id)
  (setf (slot-value self 'symbolic-id)
    (intern (symbol-name (make-symbol (format nil "F-~D" id))))))
  
(defun set-slot-value (fact slot value)
  (declare (type fact fact) (type symbol slot))
  (with-accessors ((slot-table get-slot-table)) fact
    (setf slot-table
      (acons slot value slot-table))))

(defun get-slot-value (fact slot)
  (declare (type fact fact)
           (type symbol slot))
  (cdr (assoc slot (get-slot-table fact))))
  
(defmethod get-time ((self fact))
  (get-clock self))

(defmethod update-time ((self fact) engine)
  (setf (get-clock self) (get-engine-time engine)))

(defmethod reconstruct-fact ((self fact))
  `(,(class-name (get-class self)) ,@(get-slot-source self)))

(defmethod equals ((self fact) (obj fact))
  (equal (get-class self) (get-class obj)))

(defmethod print-object ((self fact) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "f-~D (~S)" (get-fact-id self)
            (class-name (get-class self)))))

(defmethod initialize-instance :after ((self fact) &key (slots nil))
  (mapc #'(lambda (pair)
            (set-slot-value self (first pair) (second pair)))
        slots))

(defun make-fact (class slots)
  (make-instance 'fact :class class :slot-source slots :slots slots))
