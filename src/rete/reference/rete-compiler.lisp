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

;;; $Id: rete-compiler.lisp,v 1.3 2002/08/28 19:32:36 youngde Exp $

(in-package "LISA")

(defclass rete-network ()
  ((root-nodes :initform
               (make-array 0 :adjustable t :fill-pointer t)
               :reader rete-roots)))

(defun add-root-node (rete-network class)
  (let ((node (make-node1 (make-class-test class))))
    (vector-push-extend node (rete-roots rete-network))
    node))

(defun make-rete-network ()
  (make-instance 'rete-network))

(defun pass-token (node1 token)
  (accept-token node1 token))

(defun pass-tokens-on-left (node2 tokens)
  (accept-tokens-from-left node2 tokens))

(defun pass-token-on-right (node2 token)
  (accept-token-from-right node2 token))

(defun compile-rule-into-network (rete-network patterns)
  (dolist (pattern patterns)
    (let ((node
           (add-root-node rete-network (parsed-pattern-class pattern))))
      (dolist (slot (parsed-pattern-slots pattern))
        (setf node
          (add-successor
           node (make-node1
                 (make-simple-slot-test
                  (pattern-slot-name slot)
                  (pattern-slot-value slot)))
           #'pass-token))))))

(defun make-test-network (patterns)
  (let ((network (make-rete-network)))
    (compile-rule-into-network network patterns)
    network))
