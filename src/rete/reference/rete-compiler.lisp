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

;;; $Id: rete-compiler.lisp,v 1.12 2002/09/03 15:48:13 youngde Exp $

(in-package "LISA")

(defvar *root-nodes* nil)
(defvar *leaf-nodes* nil)

(defmacro set-leaf-node (node address)
  `(setf (aref *leaf-nodes* ,address) ,node))

(defmacro leaf-node ()
  `(aref *leaf-nodes* (1- (length *leaf-nodes*))))

(defmacro left-input (address)
  `(aref *leaf-nodes* (1- ,address)))

(defmacro right-input (address)
  `(aref *leaf-nodes* ,address))

(defclass rete-network ()
  ((root-nodes :initform (make-hash-table)
               :reader rete-roots)))

(defun add-root-node (class)
  (let ((root (gethash class *root-nodes*)))
    (when (null root)
      (setf root (make-node1
                  (make-class-test class)))
      (setf (gethash class *root-nodes*) root))
    root))

(defun make-intra-pattern-node (slot)
  (make-node1
   (make-simple-slot-test
    (pattern-slot-name slot)
    (pattern-slot-value slot))))

(defun distribute-token (rete-network token)
  (maphash #'(lambda (key root-node)
               (declare (ignore key))
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
           (add-root-node (parsed-pattern-class pattern)))
          (address (parsed-pattern-address pattern)))
      (set-leaf-node node address)
      (dolist (slot (parsed-pattern-slots pattern))
        (when (simple-slot-p slot)
          (setf node
            (add-successor node (make-intra-pattern-node slot)
                           #'pass-token))
          (set-leaf-node node address))))))

(defun add-join-node-test (join-node pattern slot)
  (let ((binding (pattern-slot-slot-binding slot))
        (address (parsed-pattern-address pattern)))
    (unless (= address (binding-address binding))
      (node2-add-test
       join-node
       (make-inter-pattern-test (pattern-slot-name slot) binding)))))

(defmethod add-successor :after ((self t) node conn)
  (format t "add-successor: ~S, ~S, ~S~%" self node conn))

(defun add-inter-pattern-nodes (patterns)
  (dolist (pattern (rest patterns))
    (let ((join-node (make-node2))
          (address (parsed-pattern-address pattern)))
      (dolist (slot (parsed-pattern-slots pattern))
        (unless (simple-slot-p slot)
          (add-join-node-test join-node pattern slot)))
      (add-successor
       (left-input address) join-node #'pass-tokens-on-left)
      (add-successor
       (right-input address) join-node #'pass-token-on-right)
      (set-leaf-node join-node address))))

(defun add-terminal-node ()
  (add-successor (leaf-node) (make-terminal-node) #'pass-token))

(defun compile-rule-into-network (rete-network patterns)
  (let ((*root-nodes* (rete-roots rete-network))
        (*leaf-nodes* (make-array (length patterns))))
    (add-intra-pattern-nodes patterns)
    (add-inter-pattern-nodes patterns)
    (add-terminal-node)
    (setf (slot-value rete-network 'root-nodes) *root-nodes*)))

(defun make-test-network (patterns)
  (let ((network (make-rete-network)))
    (compile-rule-into-network network patterns)
    network))
