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

;;; File: node2.lisp
;;; Description:

;;; $Id: node2.lisp,v 1.8 2002/09/06 14:20:01 youngde Exp $

(in-package "LISA")

(defclass node2 ()
  ((successor :initform nil
              :accessor node2-successor)
   (tests :initform (list)
          :accessor node2-tests)
   (left-memory :initform (make-hash-table)
                :reader node2-left-memory)
   (right-memory :initform (make-hash-table)
                 :reader node2-right-memory)))

(defun remember-token (memory token)
  (setf (gethash (token-top-fact token) memory) token))

(defun add-tokens-to-left-memory (node2 tokens)
  (remember-token (node2-left-memory node2) tokens))

(defun add-token-to-right-memory (node2 token)
  (remember-token (node2-right-memory node2) token))

(defmethod test-and-pass-tokens ((self node2) left-tokens right-token)
  (token-push-fact left-tokens (token-top-fact right-token))
  (if (every #'(lambda (test)
                 (funcall test left-tokens))
             (node2-tests self))
      (call-successor (node2-successor self) left-tokens)
    (token-pop-fact left-tokens)))
  
(defmethod accept-tokens-from-left ((self node2) left-tokens)
  (add-tokens-to-left-memory self left-tokens)
  (loop for right-token being the hash-value of (node2-right-memory self)
      do (test-and-pass-tokens self left-tokens right-token)))

(defmethod accept-token-from-right ((self node2) right-token)
  (add-token-to-right-memory self right-token)
  (loop for left-tokens being the hash-value of (node2-left-memory self)
      do (test-and-pass-tokens self left-tokens right-token)))

(defmethod add-successor ((self node2) successor-node connector)
  (setf (node2-successor self)
    (make-successor successor-node connector)))

(defun node2-add-test (node2 test)
  (push test (node2-tests node2)))

(defun make-node2 ()
  (make-instance 'node2))

