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

;;; $Id: fact.lisp,v 1.30 2001/04/06 15:54:17 youngde Exp $

(in-package "LISA")

(defstruct (fact
             (:constructor
              create-fact (&key name)))
  (name nil :type symbol)
  (fact-id -1 :type integer)
  (symbolic-id nil)
  (slot-table nil)
  (clock 0 :type fixnum))

(defun get-fact-id (self)
  (declare (type fact self))
  (fact-fact-id self))

(defun set-fact-id (self id)
  (declare (type fact self) (type integer id))
  (setf (fact-fact-id self) id))

(defun set-slot-value (self slot-name value)
  (declare (type fact self) (type slot-name slot-name))
  (setf (aref (fact-slot-table self) (slot-name-position slot-name)) value))

(defun get-symbolic-id (self)
  (declare (type fact self))
  (let ((id (fact-symbolic-id self)))
    (when (null id)
      (setf id
        (intern (symbol-name
                 (make-symbol (format nil "F-~D" (get-fact-id self))))))
      (setf (fact-symbolic-id self) id))
    (values id)))

(defun get-slot-values (self)
  (declare (type fact self))
  (let ((meta (find-meta-class (fact-name self)))
        (table (fact-slot-table self)))
    (mapcar #'(lambda (meta-slot)
                `(,(slot-name-name meta-slot)
                  ,(aref table (slot-name-position meta-slot))))
            (meta-slot-list meta))))

(defun get-slot-value (self slot-name)
  (declare (type fact self) (type slot-name slot-name))
  (aref (fact-slot-table self) (slot-name-position slot-name)))

(defun get-time (self)
  (declare (type fact self))
  (fact-clock self))

(defmethod update-time ((self fact) engine)
  (setf (fact-clock self) (get-engine-time engine)))

(defun reconstruct-fact (self)
  (declare (type fact self))
  `(,(fact-name self) ,@(get-slot-values self)))

(defun write-fact (self strm)
  (declare (type fact self))
  (when (> (fact-fact-id self) 0)
    (print `(assert ,(reconstruct-fact self)) strm)))

(defmethod equals ((self fact) (obj fact))
  (cl:assert nil () "Why is FACT.EQUALS being called?")
  (and (equal (fact-name self) (fact-name obj))
       (equal (get-slot-table self) (get-slot-table obj))))

(defmethod print-object ((self fact) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (let ((slots (get-slot-values self)))
      (format strm "F-~D ; ~S" (fact-fact-id self) (fact-name self))
      (unless (null slots)
        (format strm " ; ~S" slots)))))

(defun make-fact (name slots)
  (let ((fact (create-fact :name name))
        (meta (find-meta-class name)))
    (setf (fact-slot-table fact)
      (make-array (meta-slot-count meta)
                  :initial-element nil))
    (mapc #'(lambda (pair)
              (set-slot-value fact (first pair) (second pair)))
          slots)
    (values fact)))

#|
(defclass fact ()
  ((class :initarg :class
         :initform nil
         :reader get-class)
   (fact-id :initform -1
            :reader get-fact-id)
   (symbolic-id :initform nil)
   (slot-table :reader get-slot-table)
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
  
(defun set-slot-value (self slot-name value)
  (declare (type fact self) (type slot-name slot-name))
  (setf (aref (get-slot-table self) (slot-name-position slot-name)) value))

(defun get-symbolic-id (self)
  (declare (type fact self))
  (let ((id (slot-value self 'symbolic-id)))
    (when (null id)
      (setf id
        (intern (symbol-name
                 (make-symbol (format nil "F-~D" (get-fact-id self))))))
      (setf (slot-value self 'symbolic-id) id))
    (values id)))

(defun get-slot-values (self)
  (declare (type fact self))
  (let ((meta (find-meta-class (get-class self)))
        (table (get-slot-table self)))
    (mapcar #'(lambda (meta-slot)
                `(,(get-name meta-slot)
                  ,(aref table (get-position meta-slot))))
            (meta-slot-list meta))))

(defun get-slot-value (self slot-name)
  (declare (type fact self) (type slot-name slot-name))
  (aref (get-slot-table self) (slot-name-position slot-name)))

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
  (cl:assert nil () "Why is FACT.EQUALS being called?")
  (and (equal (get-class self) (get-class obj))
       (equal (get-slot-table self) (get-slot-table obj))))

(defmethod print-object ((self fact) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "f-~D ; ~S ; ~S" (get-fact-id self)
            (get-class self) (get-slot-values self))))

(defmethod initialize-instance :after ((self fact) &key slots #+CLISP &allow-other-keys)
  (let ((meta (find-meta-class (get-class self))))
    (setf (slot-value self 'slot-table)
      (make-array (meta-slot-count meta)
                  :initial-element nil))
    (mapc #'(lambda (pair)
              (set-slot-value self (first pair) (second pair)))
          slots)))

(defun make-fact (class slots)
  (make-instance 'fact :class class :slots slots))
|#
