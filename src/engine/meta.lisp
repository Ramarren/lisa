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

;;; $Id: meta.lisp,v 1.39 2002/08/06 01:17:03 youngde Exp $

(in-package "LISA")

(defstruct meta-data
  (fact-map (make-hash-table))
  (class-map (make-hash-table)))

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
               :initform (make-hash-table))
   (slot-list :reader get-slot-list
              :initform (list))
   (superclasses :initarg :superclasses
                 :initform '()
                 :reader get-superclasses
                 :documentation
                 "A list of symbols representing the ancestors of the class."))
  (:documentation
   "This class represents data about facts. Every LISA fact is backed by a
  CLOS instance that was either defined by the application or internally by
  LISA (via DEFTEMPLATE). META-FACT performs housekeeping chores; mapping
  symbolic fact names to actual class names, slot names to their underlying
  SLOT-NAME representation, etc."))

(defmethod initialize-instance :after ((self meta-fact) 
                                       &key effective-slots)
  "Initializes instances of class META-FACT. SLOTS is a list of symbolic slot
  names; EFFECTIVE-SLOTS is a list of actual slot names."
  (with-slots ((table slot-table) (slot-list slot-list)) self
    (mapc #'(lambda (slot-pair)
              (setf (gethash (car slot-pair) table) (cdr slot-pair)))
          effective-slots)
    ;;(setf (gethash :object table) :object)
    (maphash #'(lambda (slot eslot)
                 (declare (ignore eslot))
                 (push slot slot-list))
             table)
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
    :effective-slots
    (mapcar #'(lambda (slot-name)
                (cons (intern (symbol-name slot-name)) slot-name))
            slots)))

(defun get-slots-for-fact (meta-fact)
  (get-slot-list meta-fact))

(defun find-effective-slot (meta-fact slot-name)
  "Finds the actual CLOS slot name as identified by the symbolic name
  SLOT-NAME."
  (let ((effective-slot (gethash slot-name (get-slot-table meta-fact))))
    (cl:assert (not (null effective-slot)) ()
      "No effective slot for symbol ~S." slot-name)
    effective-slot))

(defparameter *class-map* (make-hash-table)
  "A hash table mapping a symbolic name to its associated effective class
  name.")

(defmacro with-class-map ((map) &body body)
  `(let ((,map (meta-class-map (current-engine))))
     ,@body))
  
(defmacro with-meta-map ((map) &body body)
  `(let ((,map (meta-fact-map (current-engine))))
     ,@body))

(defun register-meta-fact (symbolic-name meta-fact)
  "Binds SYMBOLIC-NAME to a META-FACT instance."
  (with-meta-map (map)
    (setf (gethash symbolic-name map) meta-fact)))

(defun forget-meta-fact (symbolic-name)
  "Forgets the association between SYMBOLIC-NAME and a META-FACT instance."
  (with-meta-map (map)
    (remhash symbolic-name map)))

(defun forget-meta-facts ()
  "Forgets all associations in the META-FACT dictionary."
  (with-meta-map (map)
    (clrhash map)))

(defun has-meta-factp (symbolic-name)
  "See if SYMBOLIC-NAME has an associated META-FACT instance."
  (with-meta-map (map)
    (gethash symbolic-name map)))
  
(defun find-meta-fact (symbolic-name &optional (errorp t))
  "Locates the META-FACT instance associated with SYMBOLIC-NAME. If ERRORP is
  non-nil, signals an error if no binding is found."
  (with-meta-map (map)
    (let ((meta-fact (gethash symbolic-name map)))
      (when errorp
        (cl:assert (not (null meta-fact)) nil
          "This fact name does not have a registered meta class: ~S"
          symbolic-name))
      meta-fact)))

(defun register-external-class (symbolic-name class)
  (with-class-map (map)
    (setf (gethash (class-name class) map) symbolic-name)))

(defun find-symbolic-name (instance)
  (with-class-map (map)
    (let ((name (gethash (class-name (class-of instance)) map)))
      (when (null name)
        (environment-error
         "The class of this instance is not known to LISA: ~S." instance))
      name)))

(defmacro import-class (class-name use-inheritancep)
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
                     (if ,use-inheritancep
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
