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

;;; File: node1-neq.lisp
;;; Description: A one-input node that tests a slot's value and negates the
;;; result.

;;; $Id: node1-neq.lisp,v 1.3 2001/03/14 18:54:36 youngde Exp $

(in-package :lisa)

(defclass node1-neq (node1)
  ((value :initarg :value
          :initform :nil
          :reader get-value)
   (slot :initarg :slot
         :initform nil
         :reader get-slot))
  (:documentation
   "A one-input node that tests a slot's value and negates the result."))

(defmethod call-node-right ((self node1-neq) token)
  (with-accessors ((value get-value)
                   (slot get-slot)) self
    (cond ((call-next-method self token)
           (values nil))
          ((not (equal (get-slot-value (get-top-fact token) slot) value))
           (pass-along self token)
           (values t))
          (t
           (values nil)))))

(defmethod equals ((self node1-neq) (obj node1-neq))
  (and (eql (get-slot self) (get-slot obj))
       (equal (get-value self) (get-value obj))))

(defmethod print-object ((self node1-neq) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(slot = ~S, value = ~S)"
            (get-slot self) (get-value self))))

(defun make-node1-neq (slot value)
  (make-instance 'node1-neq :slot slot :value value))
