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

;;; File: not-pattern.lisp
;;; Description: Small class that represents NOT conditional elements.

;;; $Id: not-pattern.lisp,v 1.1 2000/12/13 18:02:28 youngde Exp $

(in-package :lisa)

(defclass not-pattern (pattern)
  ()
  (:documentation
   "A small class that represents NOT conditional elements."))

(defmethod has-binding-p ((self not-pattern))
  (values nil))

(defun make-not-pattern (head body location)
  (make-instance 'not-pattern
    :name head :location location :slot-list body))
                 
