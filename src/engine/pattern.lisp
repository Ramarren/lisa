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

;;; $Id: pattern.lisp,v 1.52 2001/04/19 20:24:11 youngde Exp $

(in-package "LISA")

(defclass pattern ()
  ((name :initarg :name
         :reader get-name)
   (location :initarg :location
             :reader get-location))
  (:documentation
   "Base class for all types of patterns found on a rule LHS."))

(defstruct parsed-pattern
  (pattern nil :type list)
  (binding nil :type symbol)
  (type nil :type symbol))
