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

;;; File: generics.lisp
;;; Description: Common generic functions.

;;; $Id: generics.lisp,v 1.6 2001/04/19 17:25:49 youngde Exp $

(in-package "LISA")

(defgeneric equals (object-1 object-2))

(defgeneric tell-lisa-modified-instance (instance slot)
  (:method (instance slot) t)
  (:documentation
   "This generic function serves as a notification protocol, whereby LISA
   informs an application whenever a rule firing modifies an instance
   slot. Interested code should provide its own specialized method(s)."))

