;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: test1.lisp
;;; Description: This class represents a single test in a conditional element.

;;; $Id: test1.lisp,v 1.1 2000/10/24 18:55:57 youngde Exp $

(in-package "LISA")

(defclass test1 ()
  ((value :initform nil
          :initarg :value
          :reader get-value))
  (:documentation
   "Represents a single test in a conditional element."))

(defmethod do-test ((test test1))
  (not (null (get-value test))))

(defun make-test1 (value)
  (make-instance 'test1 :value value))
