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

;;; File: test-pattern.lisp
;;; Description: This class represents the TEST conditional element.

;;; $Id: test-pattern.lisp,v 1.4 2002/07/29 17:24:56 youngde Exp $

(in-package "LISA")

(defclass test-pattern (pattern)
  ((form :initarg :form
          :reader get-form))
  (:documentation
   "This class represents the TEST conditional element."))

(defmethod finalize-pattern ((self test-pattern) global-bindings)
  (let ((bindings (list)))
    (flet ((add-local-binding (var)
             (let ((binding (lookup-binding global-bindings var)))
               (cl:assert (not (null binding)) ()
                 "No global binding for variable ~S." var)
               (pushnew binding bindings))))
      (mapc #'add-local-binding
            (utils:collect #'(lambda (obj) (variablep obj))
                           (utils:flatten (get-form self)))))
    (setf (slot-value self 'bindings) bindings)
    (values self)))

(defun make-test-pattern (form location)
  (make-instance 'test-pattern
                 :name 'test :location location :form form))
