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

;;; $Id: pattern.lisp,v 1.17 2001/01/08 16:40:05 youngde Exp $

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

(defmethod add-slot ((self pattern) slot-name tests)
  (with-accessors ((slots get-slots)) self
    (setf slots
      (nconc slots `(,(make-slot slot-name tests))))))

(defmethod get-slot-count ((self pattern))
  (length (get-slots self)))

(defun canonicalize-slot (slot)
  (flet ((make-slot-variable ()
           (intern (make-symbol (format nil "?~A" (gensym)))))
         (rewrite-constraint (var value negated)
           (if negated
               `(not (equal ,var ,value))
             `(equal ,var ,value))))
    (let ((value (second slot))
          (constraint (third slot)))
      (cond ((literalp value)
             (let ((var (make-slot-variable)))
               (values var (rewrite-constraint var value nil) t)))
            ((negated-constraintp value)
             (let ((var (make-slot-variable)))
               (values var (rewrite-constraint var (second value) t) t)))
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
