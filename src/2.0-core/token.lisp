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

;;; $Id: token.lisp,v 1.9 2002/09/10 19:34:44 youngde Exp $

(in-package "LISA")

(defclass token ()
  ((facts :initform
          (make-array 0 :adjustable t :fill-pointer t)
          :reader token-facts)))

(defmethod initialize-instance :after ((self token) &key fact)
  (unless (null fact)
    (vector-push-extend fact (slot-value self 'facts)))
  self)

(defclass add-token (token) ())
(defclass remove-token (token) ())

(defun token-find-fact (token address)
  (aref (slot-value token 'facts) address))

(defun token-top-fact (token)
  (with-slots ((fact-vector facts)) token
    (aref fact-vector (1- (length fact-vector)))))

(defun token-push-fact (token fact)
  (vector-push-extend fact (slot-value token 'facts)))

(defun token-pop-fact (token)
  (with-slots ((fact-vector facts)) token
    (unless (zerop (fill-pointer fact-vector))
      (prog1
          (aref fact-vector (1- (fill-pointer fact-vector)))
        (decf (fill-pointer fact-vector))))))

(defun replicate-token (token)
  (let ((new-token (make-instance (class-of token))))
    (with-slots ((new-fact-vector facts)) new-token
      (with-slots ((existing-fact-vector facts)) token
        (dotimes (i (length existing-fact-vector))
          (vector-push-extend (aref existing-fact-vector i) new-fact-vector))))
    new-token))

(defun make-add-token (fact)
  (make-instance 'add-token :fact fact))

(defun make-remove-token (fact)
  (make-instance 'remove-token :fact fact))

