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

;;; File: node2-not.lisp
;;; Description:

;;; $Id: node2-not.lisp,v 1.11 2002/09/30 16:37:01 youngde Exp $

(in-package "LISA")

(defclass node2-not (join-node) ())

#+ignore
(defmethod test-tokens ((self node2-not) left-tokens right-token)
  (let ((tests (join-node-tests self)))
    (setf (token-not-counter left-tokens) 0)
    (token-push-fact left-tokens (token-top-fact right-token))
    (when (or (endp tests)
              (some #'(lambda (test)
                        (funcall test left-tokens))
                    tests))
      (incf (token-not-counter left-tokens)))
    (token-pop-fact left-tokens)
    (zerop (token-not-counter left-tokens))))

#+ignore
(defmethod test-tokens ((self node2-not) left-tokens right-token)
  (let ((tests (join-node-tests self)))
    (token-push-fact left-tokens (token-top-fact right-token))
    (prog1
        (or (endp tests)
            (some #'(lambda (test)
                      (funcall test left-tokens))
                  tests))
      (token-pop-fact left-tokens))))

#+ignore
(defmethod test-tokens ((self node2-not) left-tokens 
                        (right-token remove-token))
  (let ((tests (join-node-tests self)))
    (setf (token-not-counter left-tokens) 0)
    (token-push-fact left-tokens (token-top-fact right-token))
    (unless (or (endp tests)
                (some #'(lambda (test)
                          (funcall test left-tokens))
                      tests))
      (incf (token-not-counter left-tokens)))
    (token-pop-fact left-tokens)
    (zerop (token-not-counter left-tokens))))

(defmethod test-against-right-memory ((self node2-not) left-tokens)
  (loop for right-token being the hash-value 
      of (join-node-right-memory self)
      do (when (test-tokens self left-tokens right-token)
           (token-increment-not-counter left-tokens)))
  (unless (token-negated-p left-tokens)
    (pass-tokens-to-successor 
     self (combine-tokens left-tokens t))))

(defmethod test-against-left-memory ((self node2-not) 
                                     (right-token add-token))
  (loop for left-tokens being the hash-value 
      of (join-node-left-memory self)
      do (when (test-tokens self left-tokens right-token)
           (token-increment-not-counter left-tokens)
           (pass-tokens-to-successor 
            self (combine-tokens (make-remove-token left-tokens) t)))))
  
(defmethod test-against-left-memory ((self node2-not) 
                                     (right-token remove-token))
  (loop for left-tokens being the hash-value 
      of (join-node-left-memory self)
      do (when (test-tokens self left-tokens right-token)
           (token-decrement-not-counter left-tokens)
           (unless (token-negated-p left-tokens)
             (pass-tokens-to-successor self (combine-tokens left-tokens t))))))
  
(defmethod accept-tokens-from-left ((self node2-not) (left-tokens add-token))
  (add-tokens-to-left-memory self left-tokens)
  (test-against-right-memory self left-tokens))

(defmethod accept-tokens-from-left ((self node2-not) (left-tokens remove-token))
  (when (remove-tokens-from-left-memory self left-tokens)
    (pass-tokens-to-successor self (combine-tokens left-tokens t))))

(defmethod accept-token-from-right ((self node2-not) (right-token add-token))
  (add-token-to-right-memory self right-token)
  (test-against-left-memory self right-token))

(defmethod accept-token-from-right ((self node2-not) (right-token remove-token))
  (when (remove-token-from-right-memory self right-token)
    (test-against-left-memory self right-token)))

(defun make-node2-not ()
  (make-instance 'node2-not))
