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

;;; File: node2-or.lisp
;;; Description:

;;; $Id: node2-or.lisp,v 1.1 2002/10/30 15:49:54 youngde Exp $

(in-package "LISA")

(defclass node2-or (join-node) ())

(defmethod test-tokens :around ((self node2-or) left-tokens right-token)
  (let ((test-results (call-next-method)))
    (cond (test-results t)
          ((typep (successor-node (join-node-successor self)) 'node2-or)
           t)
          (t nil))))
  
(defmethod test-against-right-memory ((self node2) left-tokens)
  (loop for right-token being the hash-value 
      of (join-node-right-memory self)
      do (when (test-tokens self left-tokens right-token)
           (pass-tokens-to-successor 
            self (combine-tokens left-tokens right-token)))))

(defmethod test-against-left-memory ((self node2) (right-token add-token))
  (loop for left-tokens being the hash-value 
      of (join-node-left-memory self)
      do (when (test-tokens self left-tokens right-token)
           (pass-tokens-to-successor 
            self (combine-tokens left-tokens right-token)))))
  
(defmethod test-against-left-memory ((self node2) (right-token remove-token))
  (loop for left-tokens being the hash-value 
      of (join-node-left-memory self)
      do (when (test-tokens self left-tokens right-token)
           (pass-tokens-to-successor
            self (combine-tokens
                  (make-remove-token left-tokens) right-token)))))
  
(defmethod accept-tokens-from-left ((self node2) (left-tokens add-token))
  (add-tokens-to-left-memory self left-tokens)
  (test-against-right-memory self left-tokens))

(defmethod accept-token-from-right ((self node2) (right-token add-token))
  (add-token-to-right-memory self right-token)
  (test-against-left-memory self right-token))

(defmethod accept-tokens-from-left ((self node2) (left-tokens remove-token))
  (when (remove-tokens-from-left-memory self left-tokens)
    (test-against-right-memory self left-tokens)))

(defmethod accept-token-from-right ((self node2) (right-token remove-token))
  (when (remove-token-from-right-memory self right-token)
    (test-against-left-memory self right-token)))

(defun make-node2-or ()
  (make-instance 'node2-or))

