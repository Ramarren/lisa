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

;;; $Id: meta.lisp,v 1.11 2002/11/15 16:36:04 youngde Exp $

(in-package "LISA")

(defun get-class-name (meta-object)
  (fact-meta-object-class-name meta-object))

(defun get-slot-list (meta-object)
  (fact-meta-object-slot-list meta-object))

(defun get-superclasses (meta-object)
  (fact-meta-object-superclasses meta-object))

(defun find-meta-fact (symbolic-name &optional (errorp t))
  "Locates the META-FACT instance associated with SYMBOLIC-NAME. If ERRORP is
  non-nil, signals an error if no binding is found."
  (let ((meta-fact (find-meta-object (inference-engine) symbolic-name)))
    (when errorp
      (cl:assert (not (null meta-fact)) nil
        "This fact name does not have a registered meta class: ~S"
        symbolic-name))
    meta-fact))

(defun acquire-meta-data (actual-name)
  (labels ((build-meta-object (class direct-superclasses)
             (let* ((class-name (class-name class))
                    (meta-data
                     (make-fact-meta-object
                      :class-name class-name
                      :slot-list (reflect:class-slot-list class)
                      :superclasses direct-superclasses)))
               (register-meta-object (inference-engine) class-name meta-data)
               meta-data))
           (examine-class (class-object)
             (let ((superclasses
                    (if *consider-taxonomy-when-reasoning*
                        (reflect:find-direct-superclasses class-object)
                      nil)))
               (build-meta-object class-object superclasses)
               (dolist (super superclasses)
                 (examine-class super)))))
    (examine-class (find-class actual-name))))

(defun import-class-specification (class-name)
  (let ((class-object (find-class class-name)))
    (intern (symbol-name class-name))
    (dolist (slot-name (reflect:class-slot-list class-name))
      (intern (symbol-name slot-name)))
    (when *consider-taxonomy-when-reasoning*
      (dolist (ancestor (reflect:find-direct-superclasses class-object))
        (import-class-specification ancestor)))
    class-object))

(defconstant +no-meta-data-reason+
    "LISA doesn't know about the template named by (~S). Either the name was
    mistyped or you forgot to write a DEFTEMPLATE specification for it.")

(defun ensure-meta-data-exists (class-name)
  (flet ((ensure-class-definition ()
           (loop
             (when (find-class class-name nil)
               (acquire-meta-data class-name)
               (return))
             (cerror "Enter a template definition now."
                     +no-meta-data-reason+ class-name)
             (format t "Enter a DEFTEMPLATE form: ")
             (eval (read))
             (fresh-line))))
    (let ((meta-data (find-meta-object (inference-engine) class-name)))
      (when (null meta-data)
        (ensure-class-definition)
        (setf meta-data 
          (find-meta-object (inference-engine) class-name)))
      meta-data)))
