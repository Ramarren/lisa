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

;;; File: rete-compiler.lisp
;;; Description:

;;; $Id: rete-compiler.lisp,v 1.5 2002/08/29 19:21:54 youngde Exp $

(in-package "LISA")

(defvar *root-nodes* nil)
(defvar *terminals* nil)
(defvar *shared-nodes* nil)

(defmacro add-new-root (node)
  `(vector-push-extend ,node *root-nodes*))

(defmacro add-new-terminal (node)
  `(vector-push-extend ,node *terminals*))

(defclass rete-network ()
  ((root-nodes :initform nil
               :reader rete-roots)
   (shared-nodes :initform (make-hash-table :test #'equal)
                 :reader rete-shared-nodes)))

(defun add-root-node (class)
  (let* ((key `(:class ,class))
         (node (gethash key *shared-nodes*)))
    (when (null node)
      (setf node (make-node1 (make-class-test class)))
      (add-new-root node)
      (setf (gethash key *shared-nodes*) node))
    node))

(defun add-intra-pattern-node (slot)
  (let* ((key `(,(pattern-slot-name slot)
                ,(pattern-slot-value slot)))
         (node (gethash key *shared-nodes*)))
    (when (null node)
      (setf node (make-node1
                  (make-simple-slot-test (first key) (second key))))
      (setf (gethash key *shared-nodes*) node))
    node))

(defun distribute-token (rete-network token)
  (map nil #'(lambda (root-node)
               (accept-token root-node token))
       (rete-roots rete-network)))

(defun make-rete-network ()
  (make-instance 'rete-network))

;;; The following three functions serve as "connectors" between any two
;;; nodes. PASS-TOKEN connects two pattern (one-input) nodes, or a join node
;;; to a terminal node; PASS-TOKENS-ON-LEFT connects either a pattern node to
;;; a join node, or two join nodes; PASS-TOKEN-ON-RIGHT connects a pattern
;;; node to a join node.

(defun pass-token (node token)
  (accept-token node token))

(defun pass-tokens-on-left (node2 tokens)
  (accept-tokens-from-left node2 tokens))

(defun pass-token-on-right (node2 token)
  (accept-token-from-right node2 token))

;;; end connector functions

(defun add-intra-pattern-nodes (patterns)
  (dolist (pattern patterns)
    (let ((node
           (add-root-node (parsed-pattern-class pattern))))
      (dolist (slot (parsed-pattern-slots pattern))
        (setf node
          (add-successor 
           node (add-intra-pattern-node slot)
           #'pass-token)))
      (add-new-terminal node))))

(defun compile-rule-into-network (rete-network patterns)
  (let ((*root-nodes* (make-array 0 :adjustable t :fill-pointer t))
        (*terminals* (make-array 0 :adjustable t :fill-pointer t))
        (*shared-nodes* (rete-shared-nodes rete-network)))
    (add-intra-pattern-nodes patterns)
    (setf (slot-value rete-network 'root-nodes) *root-nodes*)
    (map nil #'(lambda (terminal)
                 (add-successor terminal (make-terminal-node) #'pass-token))
         *terminals*)))

(defun make-test-network (patterns)
  (let ((network (make-rete-network)))
    (compile-rule-into-network network patterns)
    network))
