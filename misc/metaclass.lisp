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

;;; File: metaclass.lisp
;;; Description:

;;; $Id: metaclass.lisp,v 1.2 2002/11/22 20:05:04 youngde Exp $

(in-package "CL-USER")

(defclass standard-kb-class (standard-class) ())

(defclass standard-kb-object ()
  ((shared-initialize :initform t))
  (:metaclass standard-kb-class))

(defmethod shared-initialize :after ((self standard-kb-object) slot-names &rest initargs)
  (setf (slot-value self 'shared-initialize) nil))

(defmethod (setf slot-value-using-class) :around 
           (new-value (self standard-kb-class) instance slot)
  (break)
  (let ((initializing-p (slot-value instance 'shared-initialize)))
    (call-next-method)
    (if initializing-p
        (format t "Instance ~S during initialization." instance)
      (format t "Instance ~S setting slot ~S~%" instance slot))
    new-value))

(defclass frodo (standard-kb-object)
  ((name :initarg :name
         :initform nil
         :accessor frodo-name))
  (:metaclass standard-kb-class))

