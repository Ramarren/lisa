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

;;; File: meta.lisp
;;; Description: Meta operations that LISA uses to support the manipulation of
;;; facts and instances.

;;; $Id: meta.lisp,v 1.28 2001/05/03 19:55:09 youngde Exp $

(in-package "LISA")

(defclass meta-fact ()
  ((symbolic-name :initarg :symbolic-name
                  :reader get-name)
   (class-name :initarg :class-name
               :reader get-class-name)
   (slots :initform (make-hash-table)
          :reader get-slots)))

(defun find-meta-slot (self slot-name &optional (errorp t))
  (declare (type meta-fact self))
  (let ((slot (gethash slot-name (get-slots self))))
    (when (and (null slot) errorp)
      (environment-error
       "The class ~S has no meta slot named ~S."
       (get-class-name self) slot-name))
    (values slot)))

(defun has-meta-slot-p (self slot-name)
  (declare (type meta-fact self))
  (find-meta-slot self slot-name nil))
  
(defun meta-slot-count (self)
  (declare (type meta-fact self))
  (hash-table-count (get-slots self)))

(defun meta-slot-list (self)
  (declare (type meta-fact self))
  (let ((slots '()))
    (maphash #'(lambda (key slot-name)
                 (declare (ignore key))
                 (push slot-name slots))
             (get-slots self))
    (values slots)))

(defmethod initialize-instance :after ((self meta-fact) &key slots)
  (let ((slot-table (get-slots self))
        (position -1))
    (mapc #'(lambda (slot-name)
              (setf (gethash slot-name slot-table)
                (make-slot-name slot-name (incf position))))
          slots)))

(defun make-meta-fact (name class-name slots)
  (make-instance 'meta-fact :symbolic-name name
                 :class-name class-name :slots slots))

(defclass meta-shadow-fact (meta-fact)
  ((effective-slots :reader get-effective-slots))
  (:documentation
   "This class represents meta information for the class shadow-fact."))

(defmethod initialize-instance :after ((self meta-shadow-fact) &key effective-slots)
  (let ((slot-table (make-hash-table)))
    (mapc #'(lambda (slot)
              (setf (gethash (intern (symbol-name slot)) slot-table) slot))
          effective-slots)
    (setf (slot-value self 'effective-slots) slot-table)))
    
(defmethod find-effective-slot ((self meta-shadow-fact) (slot-name symbol))
  (let ((effective-slot (gethash slot-name (get-effective-slots self))))
    (cl:assert (not (null effective-slot)) ()
      "No effective slot for symbol ~S." slot-name)
    (values effective-slot)))

(defmethod find-effective-slot ((self meta-shadow-fact) (slot-name slot-name))
  (find-effective-slot self (slot-name-name slot-name)))

(defun make-meta-shadow-fact (symbolic-name class-name slots)
  (make-instance 'meta-shadow-fact
                 :symbolic-name symbolic-name
                 :class-name class-name
                 :slots
                 (append
                  (mapcar #'(lambda (slot)
                              (intern (symbol-name slot)))
                          slots)
                  `(,:object))
                 :effective-slots slots))

(defparameter *meta-map* (make-hash-table))
(defparameter *class-map* (make-hash-table))

(defun register-meta-class (name meta-object)
  (setf (gethash name *meta-map*) meta-object))

(defun forget-meta-class (name)
  (remhash name *meta-map*))

(defun forget-meta-classes ()
  (clrhash *meta-map*))

(defun has-meta-classp (name)
  (gethash name *meta-map*))
  
(defun find-meta-class (name &optional (errorp t))
  (let ((meta-object (gethash name *meta-map*)))
    (when (and (null meta-object) errorp)
      (environment-error
       "This fact name does not have a registered meta class: ~S" name))
    (values meta-object)))

(defun register-external-class (symbolic-name class)
  (setf (gethash (class-name class) *class-map*) symbolic-name))

(defun find-symbolic-name (instance)
  (let ((name (gethash (class-name (class-of instance)) *class-map*)))
    (when (null name)
      (environment-error
       "The class of this instance is not known to LISA: ~S." instance))
    (values name)))

(defun generate-internal-methods (class)
  (eval
   `(defmethod mark-instance-as-changed ((self ,(class-name class)) &optional (slot-id nil))
     (map-clos-instances #'mark-clos-instance-as-changed self slot-id)
     (values t))))

(defun import-class (symbolic-name class slot-specs)
  (let ((meta (make-meta-shadow-fact
               symbolic-name (class-name class) slot-specs)))
    (register-meta-class symbolic-name meta)
    (register-external-class symbolic-name class)
    (generate-internal-methods class)
    (values meta)))

(defun create-class-template (name slots)
  (let* ((class (eval `(defclass ,name (deftemplate) ())))
         (meta (make-meta-fact name (class-name class) slots)))
    (register-meta-class name meta)
    (values class)))
  
(let ((initial-fact nil)
      (clear-fact nil)
      (not-or-test-fact nil))

  (defun make-special-fact (class-name)
    (create-class-template class-name '())
    (make-fact class-name nil))
    
  (defun make-initial-fact ()
    (when (null initial-fact)
      (setf initial-fact (make-special-fact 'initial-fact)))
    (values initial-fact))

  (defun make-clear-fact ()
    (when (null clear-fact)
      (setf clear-fact (make-special-fact 'clear-fact)))
    (values clear-fact))

  (defun make-not-or-test-fact ()
    (when (null not-or-test-fact)
      (setf not-or-test-fact (make-special-fact 'not-or-test-fact)))
    (values not-or-test-fact)))

(defun find-class-slots (class)
  (reflect:class-slot-list class))
