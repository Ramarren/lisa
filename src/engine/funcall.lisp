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
;;; code.

;;; $Id: funcall.lisp,v 1.2 2001/01/04 22:05:18 youngde Exp $

(in-package :lisa)

(defclass funcall ()
  ((forms :initarg :forms
          :reader get-forms)
   (bindings :initarg :bindings
             :reader get-bindings))
  (:documentation
   "This class manages the mechanics of executing arbitrary Lisp code."))

(defun create-function-context (funcall token)
  (labels ((make-lexical-binding (binding token)
             (let ((fact (find-fact token (get-location binding))))
               (cl:assert (not (null fact)) ()
                 "No fact for location ~D." (get-location binding))
               `(,(get-name binding) ,(get-slot-value fact (get-slot-name binding)))))
           (make-context ()
           `(lambda ()
              (let (,@(mapcar #'(lambda (binding)
                                  (make-lexical-binding binding))
                              (get-bindings funcall)))
                ,@(get-forms funcall)))))
    (eval (make-context))))

(defmethod evaluate ((self funcall) token)
  (funcall (create-function-context self token)))

(defun make-funcall (forms bindings)
  (make-instance 'funcall :forms forms :bindings bindings))

