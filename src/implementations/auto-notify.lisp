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

;;; File: auto-notify.lisp
;;; Description: Lisp-specific implementation of LISA's auto-notification
;;; mechanism, whereby changes to the slot values of CLOS instances, outside
;;; of LISA's control, are picked up via the MOP protocol and synchronized
;;; with KB facts.

;;; $Id: auto-notify.lisp,v 1.1 2002/11/25 15:33:45 youngde Exp $

(in-package "CL-USER")

(defclass standard-kb-object () ())

(defmethod shared-initialize :around ((self standard-kb-object) 
                                      slot-names &rest initargs)
  (let ((*ignore-this-instance* self))
    (call-next-method)))

(defmethod (setf mop:slot-value-using-class) :after
           (new-value class (instance standard-kb-object) slot)
  (flet ((ignore-instance (object)
           (and (boundp *ignore-this-instance*)
                (eq instance *ignore-this-instance*))))
    (unless (ignore-instance instance)
      (mark-instance-as-changed 
       instance :slot-id (clos:slot-definition-name slot)))))
