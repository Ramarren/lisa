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

;;; $Id: token.lisp,v 1.17 2002/09/20 21:28:45 youngde Exp $

(in-package "LISA")

(defclass token ()
  ((facts :initform
          (make-array 0 :adjustable t :fill-pointer t)
          :reader token-facts)
   (not-counter :initform 0
                :accessor token-not-counter)
   (contents :initform nil
             :reader token-contents)))

(defclass add-token (token) ())
(defclass remove-token (token) ())

(defun token-make-fact-list (token)
  (let ((facts (list))
        (vector (token-facts token)))
    (dotimes (i (length vector))
      (let ((fact (aref vector i)))
        (unless (minusp (fact-id fact))
          (push fact facts))))
    (nreverse facts)))

(defun token-find-fact (token address)
  (aref (slot-value token 'facts) address))

(defun token-top-fact (token)
  (with-slots ((fact-vector facts)) token
    (aref fact-vector (1- (length fact-vector)))))

(defun token-push-fact (token fact)
  (vector-push-extend fact (slot-value token 'facts))
  token)

(defun token-pop-fact (token)
  (with-slots ((fact-vector facts)) token
    (unless (zerop (fill-pointer fact-vector))
      (aref fact-vector (decf (fill-pointer fact-vector))))))

(defun token-terminal-contents (token)
  "Danger! This function should only be called after the token has reached its
  terminal node."
  (with-slots ((contents contents)) token
    (when (null contents)
      (setf contents (coerce (token-contents token) 'list)))
    contents))

(defun replicate-token (token &key (token-class nil))
  (let ((new-token 
         (make-instance 
             (if (null token-class)
                 (class-of token)
               (find-class token-class)))))
    (with-slots ((existing-fact-vector facts)) token
      (dotimes (i (length existing-fact-vector))
        (token-push-fact new-token (aref existing-fact-vector i))))
    new-token))

(defmethod hash-code ((self token))
  self)

(defmethod make-add-token ((fact fact))
  (token-push-fact (make-instance 'add-token) fact))

(defmethod make-remove-token ((fact fact))
  (token-push-fact (make-instance 'remove-token) fact))

(defmethod make-remove-token ((token token))
  (replicate-token token :token-class 'remove-token))

