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

;;; $Id: node2.lisp,v 1.3 2002/09/03 15:48:12 youngde Exp $

(in-package "LISA")

(defclass node2 ()
  ((successor :initform nil
              :accessor node2-successor)
   (tests :initform (list)
          :reader node2-tests)
   (left-memory :initform (make-hash-table)
                :reader node2-left-memory)
   (right-memory :initform (make-hash-table)
                 :reader node2-right-memory)))

(defmethod accept-tokens-from-left ((self node2) tokens)
  (call-successor (node2-successor self) tokens))

(defmethod accept-token-from-right ((self node2) token)
  (call-successor (node2-successor self) token))

(defmethod add-successor ((self node2) successor-node connector)
  (setf (node2-successor self)
    (make-successor successor-node connector)))

(defun node2-add-test (node2 test))

(defun make-node2 ()
  (make-instance 'node2))

