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

;;; File: fact.lisp
;;; Description: This class represents facts in the knowledge base.

;;; $Id: fact.lisp,v 1.25 2001/03/15 20:53:29 youngde Exp $

(in-package "LISA")

(defclass fact ()
  ((class :initarg :class
         :initform nil
         :reader get-class)
   (fact-id :initform -1
            :reader get-fact-id)
   (symbolic-id :reader get-symbolic-id)
   (slot-source :initarg :slot-source
                :reader get-slot-source)
   (slot-table :initform nil
               :accessor get-slot-table)
   (clock :initform 0
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
    (let ((sv (assoc slot slot-table)))
      (if (null sv)
          (setf slot-table
            (acons slot value slot-table))
        (rplacd sv value)))))

(defun get-slot-values (fact)
  (declare (type fact fact))
  (mapcar #'(lambda (slot)
              `(,(car slot) ,(cdr slot)))
          (get-slot-table fact)))

(defun get-slot-value (fact slot)
  (declare (type fact fact)
           (type symbol slot))
  (cdr (assoc slot (get-slot-table fact))))

(defmethod get-time ((self fact))
  (get-clock self))

(defmethod update-time ((self fact) engine)
  (setf (get-clock self) (get-engine-time engine)))

(defmethod reconstruct-fact ((self fact))
  `(,(class-name (get-class self)) ,@(get-slot-values self)))

(defmethod write-fact ((self fact) strm)
  (when (> (get-fact-id self) 0)
    (print `(assert ,(reconstruct-fact self)) strm)))

(defmethod equals ((self fact) (obj fact))
  (and (equal (get-class self) (get-class obj))
       (equal (get-slot-table self) (get-slot-table obj))))

(defmethod print-object ((self fact) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "f-~D ; ~S ; ~S" (get-fact-id self)
            (get-class self)
            (get-slot-values self))))

(defmethod initialize-instance :after ((self fact) &rest args)
  (declare (ignore args))
  (mapc #'(lambda (pair)
            (set-slot-value self (first pair) (second pair)))
        (get-slot-source self))
  (setf (get-slot-table self)
        (sort (get-slot-table self)
              #'(lambda (s1 s2)
                  (string< (symbol-name (car s1))
                           (symbol-name (car s2)))))))

(defun make-fact (class slots)
  (make-instance 'fact :class class :slot-source slots))
