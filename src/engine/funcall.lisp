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

;;; File: funcall.lisp
;;; Description: This class manages the mechanics of executing arbitrary Lisp
;;; code from conditional elements and rule RHSs.

;;; $Id: funcall.lisp,v 1.5 2001/01/05 17:38:06 youngde Exp $

(in-package :lisa)

(defclass function-call ()
  ((forms :initarg :forms
          :reader get-forms)
   (bindings :initarg :bindings
             :reader get-bindings))
  (:documentation
   "This class manages the mechanics of executing arbitrary Lisp code from
   conditional elements and rule RHSs."))

(defmethod make-lexical-binding ((binding pattern-binding) token)
  (let ((fact (find-fact token (get-location binding))))
    (cl:assert (not (null fact)) ()
      "No fact for location ~D." (get-location binding))
    `(,(get-name binding) ,fact)))

(defmethod make-lexical-binding ((binding slot-binding) token)
  (let ((fact (find-fact token (get-location binding))))
    (cl:assert (not (null fact)) ()
      "No fact for location ~D." (get-location binding))
    `(,(get-name binding) ,(get-slot-value fact (get-slot-name binding)))))
  
(defun create-function-context (funcall token)
  (flet ((make-context ()
           `(lambda ()
              (let (,@(mapcar #'(lambda (binding)
                                  (make-lexical-binding binding token))
                              (get-bindings funcall)))
                ,@(get-forms funcall)))))
    (eval (make-context))))

(defmethod evaluate ((self function-call) token)
  (funcall (create-function-context self token)))

(defun make-function-call (forms bindings)
  (make-instance 'function-call :forms forms :bindings bindings))

