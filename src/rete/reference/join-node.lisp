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

;;; File: join-node.lisp
;;; Description:

;;; $Id: join-node.lisp,v 1.1 2002/09/07 00:20:53 youngde Exp $

(in-package "LISA")

(defclass join-node ()
  ((successor :initform nil
              :accessor join-node-successor)
   (tests :initform (list)
          :accessor join-node-tests)
   (left-memory :initform (make-hash-table)
                :reader join-node-left-memory)
   (right-memory :initform (make-hash-table)
                 :reader join-node-right-memory)))

(defun remember-token (memory token)
  (setf (gethash (token-top-fact token) memory) token))

(defun add-tokens-to-left-memory (join-node tokens)
  (remember-token (join-node-left-memory join-node) tokens))

(defun add-token-to-right-memory (join-node token)
  (remember-token (join-node-right-memory join-node) token))

(defmethod add-successor ((self join-node) successor-node connector)
  (setf (join-node-successor self)
    (make-successor successor-node connector)))

(defmethod join-node-add-test ((self join-node) test)
  (push test (join-node-tests self)))

