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

;;; $Id: pattern.lisp,v 1.14 2001/01/05 19:46:39 youngde Exp $

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

#+ignore
(defmethod initialize-instance :after ((self pattern) &key (slot-list nil))
  (flet ((create-slot-tests (slot)
           (remove-if #'null
                      (mapcar #'(lambda (field)
                                  (unless (null field)
                                    (make-test1 field)))
                              slot))))
    (mapc #'(lambda (slot-desc)
              (add-slot self (first slot-desc)
                        (create-slot-tests (rest slot-desc))))
          slot-list)))

(defmethod initialize-instance :after ((self pattern) &key (slot-list nil))
  (labels ((create-constraint-test (constraint)
             (cond ((literalp constraint)
                    (make-test1-eq constraint))
                   ((consp constraint)
                    (if (eq (first constraint) 'not)
                        (make-test1-neq `(,(second constraint)))
                      (make-test1-eq `(,constraint))))
                   (t
                    (error "Unrecognizable slot format."))))
           (create-slot-tests (slot)
             (let* ((value (first slot))
                    (constraint (second slot))
                    (tests `(,(make-test1-eq value))))
               (unless (null constraint)
                 (setf tests
                   (append tests
                           `(,(create-constraint-test constraint)))))
               (values tests))))
    (mapc #'(lambda (slot-desc)
              (add-slot self (first slot-desc)
                        (create-slot-tests (rest slot-desc))))
          slot-list)))
