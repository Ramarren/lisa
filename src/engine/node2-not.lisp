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
;;; Description: Specialized two-input node for negated patterns.

;;; $Id: node2-not.lisp,v 1.13 2001/03/27 19:22:06 youngde Exp $

(in-package "LISA")

(defclass node2-not (node2)
  ()
  (:documentation
   "Represents a specialized two-input node for negated patterns."))

(defmethod call-node-left ((self node2-not) (token add-token))
  (call-next-method self (make-clone-token (class-of token) token)))

(defmethod pass-token-from-right ((self node2-not) left-token)
  (unless (is-negated-p left-token)
    (with-accessors ((engine get-engine)) self
      (let ((token (make-derived-token
                    (class-of left-token)
                    left-token (get-null-fact engine))))
        (update-time token engine)
        (pass-along self token))))
  (values nil))

(defmethod run-tests-vary-right ((self node2-not) (left-token add-token) tree)
  (with-tree-iterator (right-token tree)
    (when (run-tests self left-token (get-top-fact right-token))
      (increment-negation-count left-token)))
  (pass-token-from-right self left-token))

(defmethod run-tests-vary-right ((self node2-not) (left-token remove-token) tree)
  (declare (ignore tree))
  (pass-token-from-right self left-token))

(defmethod pass-token-from-left ((self node2-not) (right-token add-token) left-token)
  (with-accessors ((engine get-engine)) self
    (let ((token (make-remove-token :parent left-token
                                    :initial-fact (get-null-fact engine))))
      (update-time token engine)
      (pass-along self token)
      (increment-negation-count left-token)))
  (values nil))

(defmethod pass-token-from-left ((self node2-not) right-token left-token)
  (when (zerop (decrement-negation-count left-token))
    (with-accessors ((engine get-engine)) self
      (let ((token (make-derived-token (class-of left-token)
                                       left-token (get-null-fact engine))))
        (update-time token engine)
        (pass-along self token))))
  (values))

(defmethod run-tests-vary-left ((self node2-not) right-token tree)
  (with-tree-iterator (left-token tree)
    (when (run-tests self left-token (get-top-fact right-token))
      (pass-token-from-left self right-token left-token)))
  (values nil))

(defun make-node2-not (engine)
  (make-instance 'node2-not :engine engine))
