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

;;; File:
;;; Description:

;;; $Id: metaclass.lisp,v 1.8 2002/11/27 15:30:36 youngde Exp $

(in-package "CL-USER")

(defclass standard-kb-metaclass (standard-class) ())

(defmethod validate-superclass ((class standard-kb-metaclass)
                                (superclass standard-class))
  t)

(defmethod (setf clos:slot-value-using-class) :after
           (new-value (class standard-kb-metaclass) instance slot)
  (format t "setting slot ~S to value ~S~%" slot new-value))

(defclass frodo ()
  ((name :initarg :name
         :initform nil
         :accessor frodo-name))
  (:metaclass standard-kb-metaclass))

(defmethod (setf frodo-name) (new-value (object frodo))
  (setf (clos:slot-value-using-class (class-of object) object 'name) new-value))

(defparameter *frodo* (make-instance 'frodo :name 'frodo))
