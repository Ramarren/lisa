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

;;; $Id: network-ops.lisp,v 1.6 2002/10/02 20:00:19 youngde Exp $

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
  
(defun remove-rule-from-network (rete-network rule)
  (labels ((remove-nodes (parent nodes)
             (cl:assert (not (endp nodes)) nil
               "Somehow we ran off the end of the node list")
             (let ((node (first nodes)))
               (typecase node
               (shared-node
                (when (= (decrement-use-count node) 0)
                  (remove-node-from-parent parent node))
                (remove-nodes node (rest nodes)))
               (t nil)))))
    (remove-nodes t (rule-node-list rule))))
