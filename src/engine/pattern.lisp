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

;;; File: pattern.lisp
;;; Description:

;;; $Id: pattern.lisp,v 1.50 2001/04/03 15:07:25 youngde Exp $

(in-package "LISA")

(defstruct parsed-pattern
  (pattern nil :type list)
  (binding nil :type symbol)
  (type nil :type symbol))

(defclass pattern ()
  ((name :initarg :name
         :reader get-name)
   (slots :initarg :slot-list
          :initform nil
          :accessor get-slots)
   (bindings :initform nil
             :reader get-bindings)
   (location :initarg :location
             :reader get-location))
  (:documentation
   "Base class for all types of patterns found on a rule LHS."))

(defmethod get-slot-count ((self pattern))
  (length (get-slots self)))

(defmethod lookup-binding ((self pattern) var)
  (find var (get-bindings self) :key #'get-name))

(defmethod print-object ((self pattern) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "name ~S" (get-name self))))

(defun setup-pattern-bindings (pattern global-bindings)
  (labels ((add-global-binding (binding)
             (add-binding global-bindings
                          (make-global-slot-binding
                           (get-name binding)
                           (get-location binding)
                           (get-slot-name binding))))
           (add-new-binding (var slot)
             (let ((binding (lookup-binding global-bindings var)))
               (when (null binding)
                 (setf binding (make-local-slot-binding
                                var (get-location pattern)
                                (get-name slot)))
                 (unless (internal-bindingp binding)
                   (add-global-binding binding)))
               (pushnew binding (slot-value pattern 'bindings)
                        :key #'get-name)
               (unless (= (get-location binding) (get-location pattern))
                 (slot-has-global-binding slot))
               (values binding)))
           (add-constraint-bindings (slot)
             (mapc #'(lambda (var)
                       (add-new-binding var slot))
                   (collect #'(lambda (obj) (variablep obj))
                            (flatten (get-constraint slot))))))
    (mapc #'(lambda (slot)
              (unless (typep slot 'literal-slot)
                (add-new-binding (get-value slot) slot))
              (when (typep slot 'complex-slot)
                (add-constraint-bindings slot)))
          (get-slots pattern))))

(defun canonicalize-slots (self global-bindings)
  (declare (type pattern self))
  (let ((slots (list))
        (meta (find-meta-class (get-name self))))
    (mapc #'(lambda (slot-desc)
              (push
               (make-slot (find-meta-slot meta (first slot-desc))
                          (second slot-desc)
                          (third slot-desc)
                          global-bindings)
                    slots))
          (get-slots self))
    (setf (get-slots self) (nreverse slots))))

(defmethod finalize-pattern ((self pattern) global-bindings)
  (canonicalize-slots self global-bindings)
  (setup-pattern-bindings self global-bindings)
  (values self))
