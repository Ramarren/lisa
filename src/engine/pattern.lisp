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

;;; $Id: pattern.lisp,v 1.26 2001/01/13 21:00:29 youngde Exp $

(in-package :lisa)

(defstruct parsed-pattern
  (pattern nil :type cons)
  (binding nil :type symbol)
  (type nil :type symbol))

(defclass pattern ()
  ((name :initarg :name
         :reader get-name)
   (slots :initform nil
          :accessor get-slots)
   (locality :reader get-locality)
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

(defun is-localized-patternp (pattern)
  (declare (type (pattern pattern)))
  (get-locality pattern))

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
       
(defun canonicalize-slot (pattern slot bindings)
  (declare (type (pattern pattern) (slot slot)
                 (binding-table bindings)))
  (labels ((make-slot-variable ()
             (make-lisa-defined-slot-variable
              (intern (make-symbol (format nil "?~A" (gensym))))))
           (new-slot-binding (var)
             (unless (lookup-binding bindings var)
               (add-binding bindings
                            (make-slot-binding
                             var
                             (get-location pattern)
                             (get-name slot)))))
           (rewrite-slot (var value negated)
             (let ((varname (get-variable-name var)))
               (setf (get-value slot) varname)
               (setf (get-constraint slot)
                 (generate-test varname value negated))
               (new-slot-binding var))))
    (with-accessors ((slot-value get-value)
                     (slot-constraint get-constraint)) slot
      (cond ((literalp slot-value)
             (rewrite-slot (make-slot-variable) slot-value nil))
            ((negated-rewritable-constraintp slot-value)
             (rewrite-slot (make-slot-variable) (second slot-value) t))
            ((null slot-constraint)
             (unless (new-slot-binding slot-value)
               (rewrite-slot (make-slot-variable) slot-value nil)))
            ((literalp slot-constraint)
             (rewrite-slot slot-value slot-constraint nil))
            ((negated-rewritable-constraintp slot-constraint)
             (rewrite-slot slot-value (second slot-constraint) t))
            ((variablep slot-value)
             (new-slot-binding slot-value)))
      (values))))

(defun set-pattern-locality (pattern bindings)
  (labels ((is-localp (var)
             (let ((binding (lookup-binding bindings var)))
               (cl:assert (not (null binding)) ())
               (= (get-location binding) (get-location pattern))))
           (get-constraint-locality (constraint)
             (let ((obj (first constraint)))
               (cond ((null constraint)
                      (values t))
                     ((and (variablep obj)
                           (not (is-localp obj)))
                      (values nil))
                     (t
                      (get-constraint-locality (rest constraint))))))
           (get-slot-locality (slots)
             (let ((slot (first slots)))
               (cond ((null slot)
                      (values t))
                     ((not (is-localp (get-value slot)))
                      (values nil))
                     ((not (get-constraint-locality (get-constraint slot)))
                      (values nil))
                     (t
                      (get-slot-locality (rest slots)))))))
    (setf (slot-value pattern 'locality)
      (get-slot-locality (get-slots pattern))))
  (values))

(defmethod finalize-pattern ((self pattern) bindings)
  (mapc #'(lambda (slot)
            (canonicalize-slot self slot bindings))
        (get-slots self))
  (set-pattern-locality self bindings)
  (values self))
