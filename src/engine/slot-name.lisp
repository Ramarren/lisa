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

;;; File: slot-name.lisp
;;; Description: To assist the Rete engine in efficient slot access during
;;; pattern matching, this class binds a symbolic slot name to a small integer
;;; representing the slot's position within its fact template.

;;; $Id: slot-name.lisp,v 1.1 2001/03/15 16:55:23 youngde Exp $

(in-package "LISA")

(defclass slot-name ()
  ((name :initarg :name
         :reader get-name)
   (position :initarg :position
             :reader get-position))
  (:documentation
   "Binds a symbolic slot name to a small integer representing the slot's
   position within its fact template."))

(defmethod print-object ((self slot-name) strm)
  (print-unreadable-object (self strm)
    (format strm "~S ; ~D" (get-name self) (get-position self))))

(defun make-slot-name (name pos)
  (make-instance 'slot-name :name name :position pos))

