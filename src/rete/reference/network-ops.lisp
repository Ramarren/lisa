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

;;; File: network-ops.lisp
;;; Description:

;;; $Id: network-ops.lisp,v 1.11 2002/10/07 19:55:13 youngde Exp $

(in-package "LISA")

(defun add-token-to-network (rete-network token-ctor)
  (loop for root-node being the hash-value
      of (rete-roots rete-network)
      do (accept-token root-node (funcall token-ctor))))

(defun add-fact-to-network (rete-network fact)
  (add-token-to-network
   rete-network #'(lambda () (make-add-token fact))))

(defun remove-fact-from-network (rete-network fact)
  (add-token-to-network
   rete-network #'(lambda () (make-remove-token fact))))

(defun reset-network (rete-network)
  (add-token-to-network
   rete-network #'(lambda () (make-reset-token t))))

(defmethod decrement-use-count ((node join-node)) 0)

(defun remove-rule-from-network (rete-network rule)
  (labels ((remove-nodes (nodes)
             (if (endp nodes) rule
               (let ((node (node-pair-child (first nodes)))
                     (parent (node-pair-parent (first nodes))))
                 (when (zerop (decrement-use-count node))
                   (remove-node-from-parent rete-network parent node))
                 (remove-nodes (rest nodes))))))
    (remove-nodes (rule-node-list rule))))

(defmethod successor-exists-p ((parent shared-node) (node node1))
  (gethash (node1-test node) (shared-node-successors parent)))

(defmethod successor-exists-p (parent node)
  (declare (ignore parent node))
  nil)

(defun merge-networks (from-rete to-rete)
  (labels ((find-root-node (network node)
             (find node :key #'node1-test
                   (loop for node being the hash-value
                       of (rete-roots network)
                       collect node)))
           (merge-successors (parent successors)
             (if (endp successors) parent
               (let* ((new-successor (first successors))
                      (existing-successor
                       (successor-exists-p parent new-successor)))
                 (if (null existing-successor)
                     (add-successor 
                      parent (successor-node new-successor)
                      (successor-connector new-successor))
                   (merge-successors 
                    existing-successor 
                    (shared-node-all-successors 
                     (successor-node new-successor))))
                 (merge-successors parent (rest successors)))))
           (merge-root-node (new-root)
             (let ((existing-root
                    (find-root-node to-rete new-root)))
               (if (null existing-root)
                   (add-new-root to-rete new-root)
                 (merge-successors
                  existing-root (shared-node-all-successors new-root))))))
    (loop for new-root being the hash-value
        of (rete-roots from-rete)
        do (merge-root-node new-root))))

(defun merge-rule-into-network (to-network patterns rule)
  (let ((working-network (make-rete-network)))
    (compile-rule-into-network working-network patterns rule)
    (merge-networks working-network to-network)
    rule))
