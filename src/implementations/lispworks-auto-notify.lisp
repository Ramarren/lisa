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

;;; File: lispworks-auto-notify.lisp
;;; Description: Lispworks-specific code for LISA's auto notification
;;; mechanism, whereby changes to the slot values of CLOS instances, outside
;;; of LISA's control, are picked up via the MOP protocol and synchronized
;;; with KB facts.

;;; $Id: lispworks-auto-notify.lisp,v 1.3 2002/12/03 17:39:04 youngde Exp $

(in-package "LISA")

(defclass standard-kb-class (standard-class) ())

(defun lispworks-respond-to-slot-change (instance slot-name)
  (flet ((ignore-instance (object)
           (and (boundp '*ignore-this-instance*)
                (eq object *ignore-this-instance*))))
    (unless (ignore-instance instance)
      (mark-instance-as-changed instance :slot-id slot-name))))
  
(defmethod initialize-instance :after ((self standard-kb-class)
                                       &rest initargs) 
  (dolist (slot (class-direct-slots self))
    (dolist (writer (clos:slot-definition-writers slot))
      (let* ((gf (ensure-generic-function writer))
             (method-class
              (generic-function-method-class gf)))
        (multiple-value-bind (body initargs)
            (clos:make-method-lambda
             gf
             (class-prototype method-class)
             '(new-value object)
             nil
             `(lispworks-respond-to-slot-change
               object ',(clos:slot-definition-name slot)))
          (clos:add-method
           gf
           (apply #'make-instance method-class
                  :function (compile nil body)
                  :specializers `(,(find-class t) ,self)
                  :qualifiers '(:after)
                  :lambda-list '(value object)
                  initargs)))))))

(defmethod validate-superclass ((class standard-kb-class)
                                (superclass standard-class))
  t)

(eval-when (:load-toplevel)
  (pushnew :lisa-autonotify *features*))
