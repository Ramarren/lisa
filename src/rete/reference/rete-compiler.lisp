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

;;; $Id: rete-compiler.lisp,v 1.37 2002/10/10 15:05:37 youngde Exp $

(in-package "LISA")

(defvar *root-nodes* nil)
(defvar *rule-specific-nodes* nil)
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

(defun record-node (node parent)
  (when (typep parent 'shared-node)
    (increment-use-count parent))
  (push (make-node-pair node parent) *rule-specific-nodes*)
  node)

(defmethod remove-node-from-parent ((self rete-network) (parent t) child)
  (remhash (node1-test child) (rete-roots self)))

(defmethod remove-node-from-parent ((self rete-network) 
                                    (parent shared-node) child)
  (remove-successor parent child))

(defun make-root-node (class)
  (let* ((test (make-class-test class))
         (root (gethash test *root-nodes*)))
    (when (null root)
      (setf root (make-node1 test))
      (setf (gethash test *root-nodes*) root))
    (record-node root t)))

(defmethod add-successor ((parent t) new-node connector)
  (declare (ignore connector))
  new-node)

(defmethod add-successor :around ((parent shared-node) new-node connector)
  (declare (ignore connector))
  (record-node (call-next-method) parent))

(defun make-intra-pattern-node (slot)
  (let ((test
         (cond ((simple-slot-p slot)
                (make-simple-slot-test slot))
               ((constrained-slot-p slot)
                (make-intra-pattern-constraint-test slot))
               (t
                (make-intra-pattern-test slot)))))
    (make-node1 test)))

(defun distribute-token (rete-network token)
  (loop for root-node being the hash-value 
      of (rete-roots rete-network)
      do (accept-token root-node token)))

(defun make-rete-network ()
  (make-instance 'rete-network))

;;; The following functions serve as "connectors" between any two
;;; nodes. PASS-TOKEN connects two pattern (one-input) nodes, or a join node 
;;; to a terminal node; ENTER-JOIN-NETWORK-FROM-LEFT connects a pattern node
;;; to a join node; ENTER-JOIN-NETWORK-FROM-RIGHT also connects a pattern node
;;; to a join node; both PASS-TOKENS-ON-LEFT and PASS-TOKEN-ON-RIGHT connect
;;; two join nodes.

(defun pass-token (node token)
  (accept-token node token))

(defun pass-tokens-on-left (node2 tokens)
  (accept-tokens-from-left node2 tokens))

(defun pass-token-on-right (node2 token)
  (accept-token-from-right node2 token))

(defun enter-join-network-from-left (node2 tokens)
  (pass-tokens-on-left node2 (replicate-token tokens)))

(defun enter-join-network-from-right (node2 token)
  (pass-token-on-right node2 (replicate-token token)))

;;; end connector functions

(defun add-intra-pattern-nodes (patterns)
  (dolist (pattern patterns)
    (cond ((test-pattern-p pattern)
           (set-leaf-node t (parsed-pattern-address pattern)))
          (t
           (let ((node
                  (make-root-node (parsed-pattern-class pattern)))
                 (address (parsed-pattern-address pattern)))
             (set-leaf-node node address)
             (dolist (slot (parsed-pattern-slots pattern))
               (when (intra-pattern-slot-p slot)
                 (setf node
                   (add-successor node (make-intra-pattern-node slot)
                                  #'pass-token))
                 (set-leaf-node node address))))))))

(defun add-join-node-tests (join-node pattern)
  (labels ((add-simple-join-node-test (slot)
             (unless (= (binding-address (pattern-slot-slot-binding slot))
                        (parsed-pattern-address pattern))
               (join-node-add-test join-node
                                   (make-inter-pattern-test slot))))
           (add-slot-constraint-test (slot)
             (join-node-add-test join-node
                                 (make-predicate-test
                                  (pattern-slot-constraint slot)
                                  (pattern-slot-constraint-bindings slot)
                                  (negated-slot-p slot))))
           (add-test-pattern-predicate ()
             (join-node-add-test join-node
                                 (make-predicate-test
                                  (parsed-pattern-test-forms pattern)
                                  (parsed-pattern-test-bindings pattern))))
           (add-generic-pattern-tests ()
             (dolist (slot (parsed-pattern-slots pattern))
               (cond ((simple-bound-slot-p slot)
                      (add-simple-join-node-test slot))
                     ((constrained-slot-p slot)
                      (add-slot-constraint-test slot))))))
    (if (test-pattern-p pattern)
        (add-test-pattern-predicate)
      (add-generic-pattern-tests))
    join-node))

(defun make-join-node (pattern)
  (cond ((negated-pattern-p pattern)
         (make-node2-not))
        ((test-pattern-p pattern)
         (make-node2-test))
        (t (make-node2))))

(defun make-left-join-connection (join-node node)
  (if (typep node 'shared-node)
      (add-successor node join-node #'enter-join-network-from-left)
    (add-successor node join-node #'pass-tokens-on-left))
  join-node)

(defun make-right-join-connection (join-node node)
  (if (typep node 'shared-node)
      (add-successor node join-node #'enter-join-network-from-right)
    (add-successor node join-node #'pass-token-on-right))
  join-node)

(defun add-inter-pattern-nodes (patterns)
  (dolist (pattern (rest patterns))
    (let ((join-node (make-join-node pattern))
          (address (parsed-pattern-address pattern)))
      (add-join-node-tests join-node pattern)
      (make-left-join-connection join-node (left-input address))
      (make-right-join-connection join-node (right-input address))
      (set-leaf-node join-node address))))

(defun add-terminal-node (rule)
  (add-successor (leaf-node) (make-terminal-node rule) #'pass-token))

(defun compile-rule-into-network (rete-network patterns &optional (rule nil))
  (let ((*root-nodes* (rete-roots rete-network))
        (*rule-specific-nodes* (list))
        (*leaf-nodes* (make-array (length patterns))))
    (add-intra-pattern-nodes patterns)
    (add-inter-pattern-nodes patterns)
    (add-terminal-node rule)
    (unless (null rule)
      (attach-rule-nodes rule (nreverse *rule-specific-nodes*)))
    (setf (slot-value rete-network 'root-nodes) *root-nodes*)
    rete-network))

(defun merge-rule-into-network (to-network patterns rule &key (loader nil))
  (let ((from-network
         (compile-rule-into-network (make-rete-network) patterns rule)))
    (unless (null loader)
      (funcall loader from-network))
    (attach-rule-nodes rule (merge-networks from-network to-network))
    to-network))

(defvar *test-network* nil)

(defun make-test-network (patterns)
  (when (null *test-network*)
    (setf *test-network* (make-rete-network)))
  (compile-rule-into-network *test-network* patterns)
  *test-network*)
