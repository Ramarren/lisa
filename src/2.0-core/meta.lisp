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

;;; $Id: meta.lisp,v 1.10 2002/11/08 20:13:52 youngde Exp $

(in-package "LISA")

(defun get-class-name (meta-object)
  (fact-meta-object-class-name meta-object))

(defun get-slot-list (meta-object)
  (fact-meta-object-slot-list meta-object))

(defun get-superclasses (meta-object)
  (fact-meta-object-superclasses meta-object))

(defun find-effective-slot (meta-object slot-name)
  "Finds the actual CLOS slot name as identified by the symbolic name
  SLOT-NAME."
  (let ((effective-slot 
         (gethash slot-name (fact-meta-object-slot-table meta-object))))
    (cl:assert (not (null effective-slot)) ()
      "No effective slot for symbol ~S." slot-name)
    effective-slot))

(defun find-meta-fact (symbolic-name &optional (errorp t))
  "Locates the META-FACT instance associated with SYMBOLIC-NAME. If ERRORP is
  non-nil, signals an error if no binding is found."
  (let ((meta-fact (find-meta-object (inference-engine) symbolic-name)))
    (when errorp
      (cl:assert (not (null meta-fact)) nil
        "This fact name does not have a registered meta class: ~S"
        symbolic-name))
    meta-fact))

(defun acquire-meta-data (symbolic-name actual-name)
  (labels ((populate-slot-table (meta-object slot-list)
             (let ((slot-table (fact-meta-object-slot-table meta-object))
                   (symbol-names (list)))
               (dolist (actual-slot slot-list)
                 (let ((symbolic-slot-name actual-slot))
                   (setf (gethash symbolic-slot-name slot-table) actual-slot)
                   (push symbolic-slot-name symbol-names)))
               (setf (fact-meta-object-slot-list meta-object) symbol-names)
               meta-object))
           (import-one-class (class direct-superclasses)
             (let* ((symbolic-class-name (class-name class))
                    (meta-data
                     (make-fact-meta-object
                      :symbolic-name symbolic-class-name
                      :class-name symbolic-class-name
                      :superclasses direct-superclasses)))
               (populate-slot-table meta-data
                                    (reflect:class-slot-list class))
               (register-meta-object (inference-engine) 
                                     symbolic-class-name meta-data)
               (register-class (inference-engine) class symbolic-name)))
           (import-classes (class-object)
             (let ((superclasses
                    (if *consider-taxonomy-when-reasoning*
                        (reflect:find-direct-superclasses class-object)
                      nil)))
               (import-one-class class-object superclasses)
               (dolist (super superclasses)
                 (import-classes super)))))
    (import-classes (find-class actual-name))))

(defun import-class-specification (class-name)
  (let ((class-object (find-class class-name)))
    (intern (symbol-name class-name))
    (dolist (slot-name (reflect:class-slot-list class-name))
      (intern (symbol-name slot-name)))
    class-object))

(defconstant +no-meta-data-reason+
    "LISA doesn't know about the template named by (~S). Either the name was
    mistyped or you forgot to write a DEFTEMPLATE specification for it.")

(defun ensure-meta-data-exists (symbolic-name actual-name)
  (flet ((ensure-class-definition ()
           (loop
             (when (find-class actual-name nil)
               (acquire-meta-data symbolic-name actual-name)
               (return))
             (cerror "Enter a template definition now."
                     +no-meta-data-reason+ actual-name)
             (format t "Enter a DEFTEMPLATE form: ")
             (eval (read))
             (fresh-line))))
    (let ((meta-data (find-meta-object (inference-engine) symbolic-name)))
      (when (null meta-data)
        (ensure-class-definition)
        (setf meta-data 
          (find-meta-object (inference-engine) symbolic-name)))
      meta-data)))
