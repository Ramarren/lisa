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

;;; File: test2-neq.lisp
;;; Description: This class holds an individual, negated test performed by
;;; two-input nodes.

;;; $Id: test2-neq.lisp,v 1.3 2001/03/15 16:00:31 youngde Exp $

(in-package "LISA")

(defclass test2-neq (test)
  ((fact-location :initarg :fact-location
                  :reader get-fact-location)
   (left-slot-name :initarg :left-slot-name
                   :reader get-left-slot-name)
   (right-slot-name :initarg :right-slot-name
                    :reader get-right-slot-name))
  (:documentation
   "This class holds an individual, negated test performed by two-input nodes."))

(defmethod do-test ((self test2-neq) left-token right-fact)
  (let ((left-fact (find-fact left-token (get-fact-location self))))
    (cl:assert (not (null left-fact)))
    (not (equal (get-slot-value left-fact (get-left-slot-name self))
                (get-slot-value right-fact (get-right-slot-name self))))))

(defmethod equals ((self test2-neq) (test test2-neq))
  (and (equal (get-fact-location self)
              (get-fact-location test))
       (equal (get-left-slot-name self)
              (get-left-slot-name test))
       (equal (get-right-slot-name self)
              (get-right-slot-name test))))

(defun make-test2-neq (fact-location left-slot right-slot)
  (make-instance 'test2-neq :fact-location fact-location
                 :left-slot-name left-slot
                 :right-slot-name right-slot))
