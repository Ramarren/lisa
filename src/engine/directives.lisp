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

;;; File: directives.lisp
;;; Description: Classes that represent various directives that appear within
;;; a DECLARE form.

;;; $Id: directives.lisp,v 1.1 2001/04/03 16:47:24 youngde Exp $

(in-package "LISA")

(defclass directive ()
  ()
  (:documentation
   "The base class for all LISA directives."))

(defclass salience-directive (directive)
  ((salience :initarg :salience
             :reader get-salience))
  (:documentation
   "This class represents a rule salience directive."))

(defun make-salience-directive (salience)
  (make-instance 'salience-directive :salience salience))

