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

;;; File: test1.lisp
;;; Description: The classes here represent single tests in a conditional
;;; element. They serve as nothing more than "holders" for tests waiting to be
;;; compiled by the Rete compiler.

;;; $Id: test1.lisp,v 1.9 2001/01/07 01:28:29 youngde Exp $

(in-package :lisa)

(defclass test1 ()
  ((value :initform nil
          :initarg :value
          :reader get-value))
  (:documentation
   "The standard class for non-functional tests located within conditional
   elements."))

(defmethod value-is-variable-p ((self test1))
  (variablep (get-value self)))

(defmethod value-is-predicate-p ((self test1))
  (consp (get-value self)))

(defun make-test1 (value)
  (make-instance 'test1 :value value))

(defclass test1-eval (test1)
  ()
  (:documentation
   "Represents a user-defined functional constraint within a conditional
   element."))

(defun make-test1-eval (value)
  (make-instance 'test1-eval :value value))

(defclass test1-internal-eval (test1)
  ()
  (:documentation
   "Represents a Lisa-defined functional constraint within a conditional
   element."))

(defun make-test1-internal-eval (value)
  (make-instance 'test1-internal-eval :value value))
