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
;;; Description: A non-negated, two-input node of the Rete network. Tests in
;;; this node compare slot values and types in facts from the left and right
;;; inputs.

;;; $Id: node2.lisp,v 1.35 2001/04/20 19:37:15 youngde Exp $

(in-package "LISA")

(defclass node2 (node-test)
  ((left-tree :initform (make-token-tree :sortcodep t)
              :accessor get-left-tree)
   (right-tree :initform (make-token-tree)
               :accessor get-right-tree))
  (:documentation
   "Description: A non-negated, two-input node of the Rete network. Tests in
   this node compare slot values and types in facts from the left and right
   memories."))

(defmethod add-to-left-tree ((self node2) token)
  (add-token (get-left-tree self) token))

(defmethod add-to-right-tree ((self node2) token)
  (add-token (get-right-tree self) token))

(defmethod call-node-left ((self node2) (token add-token))
  (add-to-left-tree self token)
  (run-tests-vary-right self token (get-right-tree self)))

(defmethod call-node-left ((self node2) (token clear-token))
  (clear-tree (get-left-tree self))
  (clear-tree (get-right-tree self))
  (pass-along self token)
  (values nil))

(defmethod call-node-left ((self node2) (token remove-token))
  (when (remove-token (get-left-tree self) token)
    (run-tests-vary-right self token (get-right-tree self)))
  (values t))

(defmethod call-node-right ((self node2) (token add-token))
  (add-to-right-tree self token)
  (run-tests-vary-left self token (get-left-tree self)))

(defmethod call-node-right ((self node2) (token clear-token))
  (values nil))

(defmethod call-node-right ((self node2) (token remove-token))
  (when (remove-token (get-right-tree self) token)
    (run-tests-vary-left self token (get-left-tree self)))
  (values t))

(defmethod run-tests-vary-left ((self node2) right-token tree)
  (with-tree-iterator (left-token tree)
    (when (run-tests self left-token (get-top-fact right-token))
      (pass-along self (make-derived-token 
                        (class-of right-token)
                        left-token
                        (get-top-fact right-token)))))
  (values nil))

(defmethod run-tests-vary-right ((self node2) left-token tree)
  (with-tree-iterator (right-token tree)
    (when (run-tests self left-token (get-top-fact right-token))
      (pass-along self (make-derived-token
                        (class-of left-token)
                        left-token
                        (get-top-fact right-token)))))
  (values nil))

(defmethod print-object ((self node2) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(left = ~D ; right = ~D ; tests = ~D ; use-count = ~D)"
            (token-tree-count (get-left-tree self))
            (token-tree-count (get-right-tree self))
            (get-test-count self)
            (get-use-count self))))

(defun make-node2 (engine)
  (make-instance 'node2 :engine engine))

