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

;;; File: token.lisp
;;; Description:

;;; $Id: token.lisp,v 1.3 2002/08/29 15:29:25 youngde Exp $

(in-package "LISA")

(defclass token ()
  ((facts :initform
          (make-array 0 :adjustable t :fill-pointer t))))

(defmethod initialize-instance :after ((self token) &key fact)
  (vector-push-extend fact (slot-value self 'facts))
  self)

(defclass add-token (token) ())
(defclass remove-token (token) ())

(defun make-add-token (fact)
  (make-instance 'add-token :fact fact))

(defun make-remove-token (fact)
  (make-instance 'remove-token :fact fact))

(defun find-fact-in-token (token address)
  (aref (slot-value token 'facts) address))

(defun peek-fact-in-token (token)
  (with-slots ((fact-vector facts)) token
    (aref fact-vector (1- (length fact-vector)))))

(defun push-fact-on-token (token fact)
  (vector-push-extend (slot-value token 'facts) fact))
