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

;;; File: node1-tfn.lisp
;;; Description: This class represents a single-input Rete node that evaluates
;;; a function to test a slot's value.

;;; $Id: node1-tfn.lisp,v 1.3 2001/01/12 21:14:51 youngde Exp $

(in-package :lisa)

(defclass node1-tfn (node1)
  ((slot-name :initarg :slot-name
              :reader get-slot-name)
   (predicate :initarg :predicate
              :reader get-predicate))
  (:documentation
   "This class represents a single-input Rete node that evaluates a function
   to test a slot's value."))

(defmethod call-node-right ((self node1-tfn) token)
  (cond ((call-next-method self token)
         (values nil))
        ((evaluate (get-predicate self)
                   (make-function-context token (get-top-fact token)))
         (pass-along self token)
         (values t))
        (t (values nil))))

(defmethod equals ((self node1-tfn) (obj node1-tfn))
  (and (equal (get-slot-name self) (get-slot-name obj))
       (equals (get-predicate self) (get-predicate obj))))

(defmethod print-object ((self node1-tfn) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(slot = ~S ; predicate = ~S)"
            (get-slot-name self) (get-predicate self))))

(defun make-node1-tfn (slot-name pred)
  (make-instance 'node1-tfn :slot-name slot-name :predicate pred))

  
