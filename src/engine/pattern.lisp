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

;;; $Id: pattern.lisp,v 1.20 2001/01/09 21:03:51 youngde Exp $

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

(defun canonicalize-slot (pattern slot bindings)
  (declare (type (pattern pattern) (slot slot)
                 (binding-table bindings)))
  (labels ((make-slot-variable ()
             (intern (make-symbol (format nil "?~A" (gensym)))))
           (first-occurrence (var)
             (not (lookup-binding bindings var)))
           (new-slot-binding (var)
             (unless (lookup-binding bindings var)
               (add-binding bindings
                            (make-slot-binding
                             var (get-location pattern)
                             (get-name slot)))))
           (rewrite-slot (var value negated)
             (setf (get-value slot) var)
             (if negated
                 (setf (get-constraint slot)
                   (if (quotable value)
                       `(not (equal ,var ',value))
                     `(not (equals ,var ,value))))
               (setf (get-constraint slot)
                 (if (quotable value)
                     `(equal ,var ',value)
                   `(equal ,var ,value))))
             (new-slot-binding var)))
    (with-accessors ((slot-value get-value)
                     (slot-constraint get-constraint)) slot
      (cond ((literalp slot-value)
             (rewrite-slot (make-slot-variable) slot-value nil))
            ((negated-rewritable-constraintp slot-value)
             (rewrite-slot (make-slot-variable) (second slot-value) t))
            ((null slot-constraint)
             (if (first-occurrence slot-value)
                 (new-slot-binding slot-value)
               (rewrite-slot (make-slot-variable) slot-value nil)))
            ((literalp slot-constraint)
             (rewrite-slot slot-value slot-constraint nil))
            ((negated-rewritable-constraintp slot-constraint)
             (rewrite-slot slot-value (second slot-constraint) t))
            (t
             (values slot))))))

(defmethod finalize-pattern ((self pattern) bindings)
  (mapc #'(lambda (slot)
            (canonicalize-slot self slot bindings)
            (format t "slot = ~S~%" slot))
        (get-slots self)))
