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

;;; File: node1-tect
;;; Description: A one-input node that tests the fact class type in a pattern
;;; network.

;;; $Id: node1-tect.lisp,v 1.13 2001/05/24 22:40:16 youngde Exp $

(in-package "LISA")

(defclass node1-tect (node1)
  ((class :initform nil
          :initarg :class
         :reader get-class))
  (:documentation
   "A one-input node that tests the fact class type in a pattern."))

(defmethod class-is-match-p ((self node1-tect) (fact fact))
  (eq (fact-name fact) (get-class self)))

(defmethod class-is-match-p ((self node1-tect) (fact shadow-fact))
  (let ((class (get-class self)))
    (or (eq (fact-name self) class)
        (has-superclass fact class))))

(defmethod call-node-right ((self node1-tect) (token token))
  (flet ((call-right (self token)
           (if (class-is-match-p self (get-top-fact token))
               (pass-along self token)
             (values nil))))
    (if (call-next-method self token)
        (values nil)
      (call-right self token))))

#+ignore
(defmethod call-node-right ((self node1-tect) (token token))
  (flet ((call-right (self token)
           (if (eq (fact-name (get-top-fact token))
                   (get-class self))
               (pass-along self token)
             (values nil))))
    (if (call-next-method self token)
        (values nil)
      (call-right self token))))

(defmethod equals ((self node1-tect) (obj node1-tect))
  (eq (get-class self) (get-class obj)))

(defmethod print-object ((self node1-tect) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(class = ~S)" (get-class self))))

(defun make-node1-tect (class)
  (make-instance 'node1-tect :class class))
