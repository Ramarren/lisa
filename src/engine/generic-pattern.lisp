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

;;; File: generic-pattern.lisp
;;; Description: Class representing the default style of pattern found
;;; on rule LHSs, as in (fact (slot-0 1) (slot-1 blue)).

;;; $Id: generic-pattern.lisp,v 1.9 2001/03/14 18:54:36 youngde Exp $

(in-package :lisa)

(defclass generic-pattern (pattern)
  ((bound-name :initarg :bound-name
               :initform nil
               :accessor get-bound-name))
  (:documentation
   "Represents  the default style of pattern found on rule LHSs, as in
   (fact (slot-0 1) (slot-1 blue))."))

(defmethod set-pattern-binding ((self generic-pattern) binding)
  (setf (get-bound-name self) binding))

(defmethod get-pattern-binding ((self generic-pattern))
  (get-bound-name self))

(defmethod has-binding-p ((self generic-pattern))
  (not (null (get-pattern-binding self))))

(defun make-generic-pattern (head body location &optional (bound-name nil))
  (make-instance 'generic-pattern
    :name head :location location :bound-name bound-name :slot-list body))
