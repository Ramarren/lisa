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

;;; File: node1-teq.lisp
;;; Description: A one-input node that tests a slot's value.

;;; $Id: node1-teq.lisp,v 1.2 2000/11/03 19:23:26 youngde Exp $

(in-package "LISA")

(defclass node1-teq (node1)
  ((value :initarg :value
          :initform :nil
          :reader get-value)
   (slot :initarg :slot
         :initform nil
         :reader get-slot))
  (:documentation
   "A one-input node that tests a slot's value."))

(defmethod call-node-right ((self node1-teq) token)
  (flet ((call-right (self token)
           (with-accessors ((value get-value)
                            (slot get-slot)) self
             (cond ((equals (get-slot-value (get-top-fact token) slot)
                            value)
                    (pass-along token)
                    (values t))
                   (t (values nil))))))
  (if (call-next-method self token)
      (values nil)
    (call-right self token))))

(defmethod equals ((self node1-teq) (obj node1-teq))
  (and (equal (get-slot self) (get-slot obj))
       (equal (get-value self) (get-value obj))))

(defun make-node1-teq (slot value)
  (make-instance 'node1-teq :slot slot :value value))
