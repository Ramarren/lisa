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

;;; File: test2-fn.lisp
;;; Description: A two-input node TEST that evaluates a predicate as its
;;; result.

;;; $Id: test2-fn.lisp,v 1.1 2001/01/05 21:06:14 youngde Exp $

(in-package :lisa)

(defclass test2-fn (test)
  ((slot-name :initarg :slot-name
              :reader get-slot-name)
   (predicate :initarg :predicate
              :reader get-predicate))
  (:documentation
   "A two-input node TEST that evaluates a predicate as its result."))

