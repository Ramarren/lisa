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

;;; File: shadow-fact.lisp
;;; Description: This class represents LISA facts that are actually CLOS
;;; instances.

;;; $Id: shadow-fact.lisp,v 1.3 2001/04/11 14:20:59 youngde Exp $

(in-package "LISA")

(defclass shadow-fact (fact)
  ()
  (:documentation
   "This class represents LISA facts that are actually CLOS instances."))

(defmethod initialize-instance :after ((self shadow-fact) &key instance)
  (let ((meta (get-meta-fact self)))
    (flet ((set-slot-from-instance (slot-name)
             (initialize-slot-value
              self slot-name
              (slot-value
               instance (find-effective-slot meta slot-name)))))
      (maphash #'(lambda (key slot)
                   (declare (ignore key))
                   (if (eq (slot-name-name slot) :object)
                       (initialize-slot-value self slot instance)
                     (set-slot-from-instance slot)))
               (get-slots meta)))))

(defun instance-of-shadow-fact (self)
  (declare (type shadow-fact self))
  (get-slot-value self (find-meta-slot (get-meta-fact self) :object)))

(defmethod set-slot-value :after ((self shadow-fact) slot-name value)
  (let ((meta (get-meta-fact self)))
    (format t "Setting slot-value of instance using ~S~%" meta)
    (setf (slot-value (instance-of-shadow-fact self)
                      (find-effective-slot meta slot-name))
      value)))

(defun make-shadow-fact (name instance)
  (make-instance 'shadow-fact :name name :instance instance))

