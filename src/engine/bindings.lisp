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

;;; $Id: bindings.lisp,v 1.8 2001/01/23 21:34:29 youngde Exp $

(in-package :lisa)

(defclass lisa-defined-slot-variable ()
  ((varname :initarg :varname
            :reader get-varname)))

(defmethod get-variable-name ((self lisa-defined-slot-variable))
  (get-varname self))

(defmethod get-variable-name ((obj symbol))
  (values obj))

(defun make-lisa-defined-slot-variable (name)
  (make-instance 'lisa-defined-slot-variable :varname name))

(defclass binding ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (location :initarg :location
             :initform nil
             :reader get-location))
  (:documentation
   "The base class for all types of bindings."))

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

(defclass lexical-slot-binding (slot-binding)
  ()
  (:documentation
   "This class represents a slot variable whose scope is beyond the pattern
   containing that variable."))

(defun make-lexical-slot-binding (name location slot)
  (make-instance 'lexical-slot-binding :name name :location location
                 :slot-name slot)) 

(defmethod make-slot-binding ((variable lisa-defined-slot-variable)
                              location slot-name)
  (make-local-slot-binding (get-varname variable) location slot-name))

(defmethod make-slot-binding ((variable symbol) location slot-name)
  (make-lexical-slot-binding variable location slot-name))

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
