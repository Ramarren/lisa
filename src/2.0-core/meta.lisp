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

;;; NB: A note on terminology. We make the distinction here between symbolic
;;; slot names and effective slot names. The former refers to an internal
;;; symbol, created by LISA, used to identify fact slots within rules; the
;;; latter refers to the actual, package-qualified slot name.

;;; $Id: meta.lisp,v 1.5 2002/10/16 14:59:44 youngde Exp $

(in-package "LISA")

(defclass meta-fact ()
  ((symbolic-name :initarg :symbolic-name
                  :reader get-name
                  :documentation
                  "The symbolic name for the fact, as used in rules.")
   (class-name :initarg :class-name
               :reader get-class-name
               :documentation
               "A symbol representing the fully qualified CLOS class name.")
   (slot-table :reader get-slot-table
               :initform (make-hash-table)
               :documentation
               "A hash table mapping symbolic slot names to their
                corresponding effective slot names.")
   (slot-list :reader get-slot-list
              :initform (list)
              :documentation
              "A list containing the symbolic slot names for a fact.")
   (superclasses :initarg :superclasses
                 :initform '()
                 :reader get-superclasses
                 :documentation
                 "A list of symbols representing the ancestors of the class."))
  (:documentation
   "This class represents data about facts. Every LISA fact is backed by a
  CLOS instance that was either defined by the application or internally by
  LISA (via DEFTEMPLATE). META-FACT performs housekeeping chores; mapping
  symbolic fact names to actual class names, slot names to their corresponding
  effective slot names, etc."))

(defmethod initialize-instance :after ((self meta-fact) 
                                       &key slot-pairs)
  "Initializes instances of class META-FACT. SLOT-PAIRS is a list of CONS
  cells; the CDR of each is a symbolic slot name, the CAR a corresponding
  effective slot name."
  (with-slots ((table slot-table) (slot-list slot-list)) self
    (mapc #'(lambda (slot-pair)
              (let ((slot-name (car slot-pair))
                    (effective-slot-name (cdr slot-pair)))
                (setf (gethash slot-name table) effective-slot-name)
                (push slot-name slot-list)))
          slot-pairs)
    self))

(defun make-meta-fact (name class-name superclasses slots)
  "The constructor for class META-FACT. The symbolic name assigned to the fact
  is represented by NAME; the actual CLOS class name is CLASS-NAME;
  SUPERCLASSES is a list of symbols representing the names of ancestor
  classes; SLOTS is a list of symbolic slot names."
  (make-instance 'meta-fact
    :symbolic-name name
    :class-name class-name
    :superclasses 
    (mapcar #'(lambda (superclass)
                (intern (symbol-name (class-name superclass))))
            superclasses)
    :slot-pairs
    (mapcar #'(lambda (slot-name)
                (cons (intern (symbol-name slot-name)) slot-name))
            slots)))

(defun find-effective-slot (meta-fact slot-name)
  "Finds the actual CLOS slot name as identified by the symbolic name
  SLOT-NAME."
  (let ((effective-slot (gethash slot-name (get-slot-table meta-fact))))
    (cl:assert (not (null effective-slot)) ()
      "No effective slot for symbol ~S." slot-name)
    effective-slot))

(defconstant +lisa-symbolic-name+
    '_lisa-symbolic-name_)

(defconstant +lisa-meta-data+
    '_lisa-meta-data_)

(defun register-meta-fact (symbolic-name meta-fact)
  "Binds SYMBOLIC-NAME to a META-FACT instance."
  (setf (get symbolic-name +lisa-meta-data+) meta-fact))

(defun has-meta-factp (symbolic-name)
  "See if SYMBOLIC-NAME has an associated META-FACT instance."
  (get symbolic-name +lisa-meta-data+))
  
(defun find-meta-fact (symbolic-name &optional (errorp t))
  "Locates the META-FACT instance associated with SYMBOLIC-NAME. If ERRORP is
  non-nil, signals an error if no binding is found."
  (let ((meta-fact (get symbolic-name +lisa-meta-data+)))
    (when errorp
      (cl:assert (not (null meta-fact)) nil
        "This fact name does not have a registered meta class: ~S"
        symbolic-name))
    meta-fact))

(defun register-external-class (symbolic-name class)
  (setf (get (class-name class) +lisa-symbolic-name+) symbolic-name))

(defun find-symbolic-name (instance)
  (let ((symbolic-name
         (get (class-name (class-of instance)) +lisa-symbolic-name+)))
    (cl:assert (not (null symbolic-name)) nil
      "The class of this instance is not known to LISA: ~S." instance)
    symbolic-name))

(defmacro import-class (class-name use-inheritance-p)
  "Imports an external class into LISA, making it available for
  reasoning. CLASS-NAME is the symbolic name of the class; if
  USE-INHERITANCE-P is T, the ancestors of the class are also imported and
  made available for reasoning."
  `(labels ((import-one-class (class direct-superclasses)
              (let* ((class-name (class-name class))
                     (symbolic-name
                      (intern (symbol-name class-name))))
                (unless (find-meta-fact symbolic-name nil)
                  (let ((meta (make-meta-fact
                               symbolic-name
                               class-name
                               direct-superclasses
                               (reflect:class-slot-list class))))
                    (register-meta-fact symbolic-name meta)
                    (register-external-class symbolic-name class)))))
            (import-classes (class)
              (let ((superclasses
                     (if ,use-inheritance-p
                         (reflect:find-direct-superclasses class)
                       '())))
                (import-one-class class superclasses)
                (dolist (super superclasses)
                  (import-classes super)))))
     (import-classes (find-class ',class-name))))

(defun register-template (name class)
  "Creates and remembers the meta fact instance associated with a class. NAME
  is the symbolic name of the fact as used in rules; CLASS is the CLOS class
  instance associated with the fact."
  (let ((meta-fact
         (make-meta-fact name (class-name class)
                         nil (reflect:class-slot-list class))))
    (register-meta-fact name meta-fact)
    meta-fact))
