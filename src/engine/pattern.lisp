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

;;; File: pattern.lisp
;;; Description:

;;; $Id: pattern.lisp,v 1.40 2001/01/28 20:03:26 youngde Exp $

(in-package :lisa)

(defstruct parsed-pattern
  (pattern nil :type list)
  (binding nil :type symbol)
  (type nil :type symbol))

(defclass pattern ()
  ((name :initarg :name
         :reader get-name)
   (slots :initform nil
          :accessor get-slots)
   (bindings :initform nil
             :reader get-bindings)
   (location :initarg :location
             :reader get-location))
  (:documentation
   "Base class for all types of patterns found on a rule LHS."))

(defmethod add-slot ((self pattern) slot)
  (with-accessors ((slots get-slots)) self
    (setf slots
      (nconc slots `(,slot)))))

(defmethod get-slot-count ((self pattern))
  (length (get-slots self)))

(defmethod print-object ((self pattern) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "name ~S" (get-name self))))

(defmethod initialize-instance :after ((self pattern) &key (slot-list nil))
  (mapc #'(lambda (desc)
            (add-slot
             self (make-slot (first desc) (second desc) (third desc))))
        slot-list))

(defmacro generate-test (var value negated)
  `(cond (,negated
          (if (quotablep ,value)
              `(not (eq ,,var ',,value))
            `(not (equal ,,var ,,value))))
         (t
          (if (quotablep ,value)
              `(eq ,,var ',,value)
            `(equal ,,var ,,value)))))
       
(defun canonicalize-slot (pattern slot global-bindings)
  (labels ((make-slot-variable ()
             (intern (format nil "?_~A" (symbol-name (gensym)))))
           (rewrite-slot (var value negated)
             (setf (get-value slot) var)
             (setf (get-constraint slot)
               (generate-test var value negated))))
    (with-accessors ((slot-value get-value)
                     (slot-constraint get-constraint)) slot
      (cond ((literalp slot-value)
             (change-class slot 'optimisable-slot))
            ((negated-rewritable-constraintp slot-value)
             (change-class slot 'optimisable-negated-slot))
            ;; Then the slot value must be a variable...
            ((null slot-constraint)
             (when (lookup-binding global-bindings slot-value)
               (rewrite-slot (make-slot-variable) slot-value nil)))
            ((literalp slot-constraint)
             (rewrite-slot slot-value slot-constraint nil))
            ((negated-rewritable-constraintp slot-constraint)
             (rewrite-slot slot-value (second slot-constraint) t))
            ((variablep slot-constraint)
             (rewrite-slot slot-value slot-constraint nil))))
    (values)))

(defun canonicalize-slots (pattern bindings)
  (mapc #'(lambda (slot)
            (canonicalize-slot pattern slot bindings))
        (get-slots pattern)))

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
              (unless (typep slot 'optimisable-slot)
                (add-new-binding (get-value slot) slot)
                (add-constraint-bindings slot)))
          (get-slots pattern))))

(defmethod finalize-pattern ((self pattern) global-bindings)
  (canonicalize-slots self global-bindings)
  (setup-pattern-bindings self global-bindings)
  (values self))
