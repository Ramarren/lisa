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

;;; File: bound-pattern.lisp
;;; Description: This class represents a special type of generic-pattern that
;;; can be bound to a variable.

;;; $Id: bound-pattern.lisp,v 1.1 2001/04/19 19:02:19 youngde Exp $

(in-package "LISA")

(defclass bound-pattern (generic-pattern)
  ((pattern-binding :initarg :binding
                    :reader get-bound-name))
  (:documentation
   "This class represents a special type of generic-pattern that can be bound
   to a variable."))

(defun make-bound-pattern (head body location bound-name)
  (make-instance 'bound-pattern
                 :name head :location location :slot-list body
                 :binding bound-name))

