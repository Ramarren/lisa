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

;;; $Id: shadow-fact.lisp,v 1.1 2001/04/09 16:51:31 youngde Exp $

(in-package "LISA")

(defclass shadow-fact (fact)
  ((instance :initarg :instance
             :reader get-instance))
  (:documentation
   "This class represents LISA facts that are actually CLOS instances."))

(defmethod set-slot-value :after ((self shadow-fact) slot-name value)
  (format t "setting slot-value of instance after fact.~%"))

(defun make-shadow-fact (name instance slots)
  (make-instance 'shadow-fact :name name :instance instance :slots slots))

