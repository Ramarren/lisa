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

;;; File: pattern.lisp
;;; Description: Base class for all types of patterns found on a rule LHS.

;;; $Id: pattern.lisp,v 1.3 2002/08/22 19:12:24 youngde Exp $

(in-package "LISA")

(defstruct (parsed-pattern
            (:constructor make-internal-parsed-pattern))
  (pattern nil :type list)
  (location nil :type integer)
  (binding nil :type symbol)
  (variables nil :type list)
  (type nil :type symbol))

(defun bound-pattern-p (parsed-pattern)
  (not (null (parsed-pattern-binding parsed-pattern))))

(defun make-parsed-pattern (&key pattern binding type location variables)
  (make-internal-parsed-pattern
   :pattern pattern
   :binding binding
   :type type
   :variables variables
   :location location))
