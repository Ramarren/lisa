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

;;; File: funcall.lisp
;;; Description: This class manages the mechanics of executing arbitrary Lisp
;;; code from conditional elements and rule RHSs.

;;; $Id: funcall.lisp,v 1.30 2001/04/24 20:37:13 youngde Exp $

(in-package "LISA")

(defclass function-call-context ()
  ((token :initarg :token
          :reader get-token)
   (fact :initarg :fact
         :reader get-fact)))

(defun make-function-context (token &optional (fact nil))
  (make-instance 'function-call-context :token token :fact fact))

(defclass function-call ()
  ((forms :initarg :forms
          :reader get-forms)
   (bindings :initarg :bindings
             :reader get-bindings)
   (function :reader get-function))
  (:documentation
   "This class manages the mechanics of executing arbitrary Lisp code from
   conditional elements and rule RHSs."))

(defmethod make-lexical-binding ((binding pattern-binding) context)
  (let ((fact (find-fact (get-token context) (get-location binding))))
    (cl:assert (not (null fact)) ()
      "No fact for location ~D." (get-location binding))
    (values fact)))

(defmethod make-lexical-binding ((binding global-slot-binding) context)
  (let ((fact (find-fact (get-token context) (get-location binding))))
    (cl:assert (not (null fact)) ()
      "No fact for location ~D." (get-location binding))
    (get-slot-value fact (get-slot-name binding))))
  
(defmethod make-lexical-binding ((binding local-slot-binding) context)
  (get-slot-value (get-fact context)
                  (get-slot-name binding)))

(defmethod make-lexical-binding ((binding special-binding) context)
  (declare (ignore context))
  (get-value binding))

(defun evaluate (func context)
  (declare (type function-call func)
           (type function-call-context context))
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (flet ((eval-func ()
           (let ((fcall (get-function func)))
             (declare (function fcall))
             (apply fcall
                    (mapcar #'(lambda (binding)
                                (make-lexical-binding binding context))
                            (get-bindings func))))))
    (handler-case
        (eval-func)
      (error (condition)
        (evaluation-error condition (get-forms func))))))

(defmethod equals ((self function-call) (obj function-call))
  (eq self obj))

(defmethod print-object ((self function-call) strm)
  (print-unreadable-object (self strm :type t)
    (format strm "~S" (get-forms self))))

(defmethod initialize-instance :after ((self function-call) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'function)
    (let ((lambda-list (mapcar #'get-name (get-bindings self))))
      (compile nil `(lambda (,@lambda-list)
                     (declare (special ,@lambda-list))
                     (progn ,@(get-forms self)))))))

(defun make-function-call (forms bindings)
  (make-instance 'function-call :forms forms :bindings bindings))

