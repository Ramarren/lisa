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

;;; $Id: test1.lisp,v 1.7 2001/01/04 17:03:11 youngde Exp $

(in-package :lisa)

(defclass test1 ()
  ((value :initform nil
          :initarg :value
          :reader get-value))
  (:documentation
   "The base class for tests located within conditional elements. There's no
   reason this class should be instantiated directly."))

(defmethod value-is-variable-p ((self test1))
  (variablep (get-value self)))

(defclass test1-eq (test1)
  ()
  (:documentation
   "Represents a single equality test in a conditional element."))

(defun make-test1-eq (value)
  (make-instance 'test1-eq :value value))

(defclass test1-neq (test1)
  ()
  (:documentation
   "Represents a single negated equality test in a conditional element."))

(defun make-test1-neq (value)
  (make-instance 'test1-neq :value value))
