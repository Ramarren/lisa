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

;;; $Id: fact.lisp,v 1.15 2001/01/23 21:05:00 youngde Exp $

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
   (slot-table :initform (make-hash-table)
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
  
(defmethod set-slot-value ((self fact) slot value)
  (setf (gethash slot (get-slot-table self)) value))

(defmethod get-slot-value ((self fact) slot)
  (gethash slot (get-slot-table self)))
  
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
  (let ((map (slot-value self 'slot-table)))
    (mapc #'(lambda (slot)
              (setf (gethash (first slot) map) (second slot)))
          slots)))

(defun make-fact (class slots)
  (make-instance 'fact :class class :slot-source slots :slots slots))
