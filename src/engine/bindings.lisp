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

;;; File: bindings.lisp
;;; Description: Classes in this file represent various types of
;;; variable bindings that form the lexical environment of rule
;;; right-hand-sides.

;;; $Id: bindings.lisp,v 1.9 2001/01/25 22:14:33 youngde Exp $

(in-package :lisa)

(defclass binding ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (location :initarg :location
             :initform nil
             :reader get-location)
   (internal :initform nil
             :reader get-internal))
  (:documentation
   "The base class for all types of bindings."))

(defmethod initialize-instance :after ((self binding) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'internal)
    (string= "?_" (subseq (symbol-name (get-name self)) 0 2))))

(defun internal-bindingp (binding)
  (get-internal binding))

(defmethod print-object ((self binding) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; location = ~D)"
            (get-name self) (get-location self))))

(defclass pattern-binding (binding)
  ()
  (:documentation
   "Represents a fact-assignment binding."))

(defun make-pattern-binding (name location)
  (make-instance 'pattern-binding :name name :location location))

(defclass slot-binding (binding)
  ((slot-name :initarg :slot-name
              :reader get-slot-name))
  (:documentation
   "Represents variable bindings that can occur within pattern slots."))

(defmethod print-object ((self slot-binding) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; location = ~D ; slot-name = ~S)"
            (get-name self) (get-location self) (get-slot-name self))))

(defclass local-slot-binding (slot-binding)
  ()
  (:documentation
   "This class represents a slot variable whose scope is limited to the
   pattern containing that variable."))

(defun make-local-slot-binding (name location slot)
  (make-instance 'local-slot-binding :name name :location location
                 :slot-name slot)) 

(defclass global-slot-binding (slot-binding)
  ()
  (:documentation
   "This class represents a slot variable whose scope is beyond the pattern
   containing that variable."))

(defun make-global-slot-binding (name location slot)
  (make-instance 'global-slot-binding :name name :location location
                 :slot-name slot)) 

(defclass binding-table ()
  ((table :initform (make-hash-table)
          :reader get-table))
  (:documentation
   "A small class that maintains a collection of variable bindings."))

(defmethod lookup-binding ((self binding-table) varname)
  (gethash varname (get-table self)))

(defmethod add-binding ((self binding-table) binding)
  (with-accessors ((table get-table)) self
    (unless (gethash (get-name binding) table)
      (setf (gethash (get-name binding) table) binding))))

(defun get-binding-list (table &key (test #'identity))
  (let ((bindings (list)))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (when (funcall test val)
                   (push val bindings)))
             (get-table table))
    (values bindings)))

(defun make-binding-table ()
  (make-instance 'binding-table))
