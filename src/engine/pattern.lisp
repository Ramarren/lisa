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

;;; $Id: pattern.lisp,v 1.18 2001/01/09 01:35:05 youngde Exp $

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

(defmethod initialize-instance :after ((self pattern) &key (slot-list nil))
  (mapc #'(lambda (desc)
            (add-slot
             self (make-slot (first desc) (second desc) (third desc))))
        slot-list))

(defun canonicalize-slot (rule pattern slot)
  (flet ((make-slot-variable ()
           (intern (make-symbol (format nil "?~A" (gensym)))))
         (rewrite-constraint (var value negated)
           (if negated
               `(not (equal ,var ,value))
             `(equal ,var ,value))))
    (let* ((value (second slot))
           (constraint (third slot))
           (var (if (variablep value)
                    value
                  (make-slot-variable))))
      (cond ((literalp value)
             (values var (rewrite-constraint var value nil) t))
            ((negated-constraintp value)
             (values var (rewrite-constraint var (second value) t) t))
            ((and (not (null constraint))
                  (literalp constraint))
             (values value (rewrite-constraint 
                            value constraint nil) t))
            ((and (not (null constraint))
                  (negated-constraintp constraint))
             (values value (rewrite-constraint 
                            value (second constraint) t) t))
            (t
             (values value constraint nil))))))

(defun canonicalize-slot (pattern slot bindings)
  (declare (type (pattern pattern) (slot slot)
                 (binding-table bindings)))
  (labels ((make-slot-variable ()
             (intern (make-symbol (format nil "?~A" (gensym)))))
           (new-slot-binding (var)
             (unless (lookup-binding var bindings)
               (add-binding bindings
                            (make-slot-binding
                             var (get-location pattern)
                             (get-name slot)))))
           (rewrite-slot (var value negated)
             (setf (get-value slot) var)
             (if negated
                 (setf (get-constraint slot)
                   `(not (equal ,var ,value)))
               (setf (get-constraint slot)
                 `(equal ,var ,value)))
             (new-binding-maybe var)))
    (with-accessors ((slot-value get-value)
                     (slot-constraint get-constraint)) slot
      (cond ((literalp slot-value)
             (rewrite-slot (make-slot-variable) slot-value nil))
            ((negated-constraintp slot-value)
             (rewrite-slot (make-slot-variable) (second slot-constraint) t))
            ((null slot-constraint)
             (if (first-occurrence slot-value)
                 (new-slot-binding slot-value)
               (rewrite-slot (make-slot-variable) slot-value nil)))
            ((literalp constraint)
             (rewrite-slot slot-value constraint nil))
            ((and (not (null slot-constraint))
                  (literalp constraint))
             (rewrite-slot slot-value slot-constraint nil))
            ((and (not (null slot-constraint))
                  (negated-constraintp slot-constraint))
             (rewrite-slot slot-value slot-constraint t))))))

(defmethod finalize-pattern ((self pattern) bindings)
  (mapc #'(lambda (slot)
            (canonicalize-slot pattern slot bindings))
        (get-slots self)))

#+ignore
(defmethod initialize-instance :after ((self pattern) &key (slot-list nil))
  (flet ((create-slot-tests (slot)
           (multiple-value-bind (value constraint changed)
               (canonicalize-slot slot)
             (format t "name ~S value ~S ; constraint ~S ; changed ~S~%"
                     (first slot) value constraint changed)
             (let ((tests (list (make-test1-var value))))
               (unless (null constraint)
                 (push (if changed
                           (make-test1-internal-eval constraint)
                         (make-test1-eval constraint))
                       tests))
               (values (reverse tests))))))
    (mapc #'(lambda (slot-desc)
              (add-slot self (first slot-desc)
                        (create-slot-tests slot-desc)))
          slot-list)))
