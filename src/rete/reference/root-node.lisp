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

;;; File: root-node.lisp
;;; Description:

;;; $Id: root-node.lisp,v 1.2 2002/08/30 01:41:20 youngde Exp $

(in-package "LISA")

(defclass root-node (shared-node)
  ((class :initarg :class
          :reader root-node-class)
   (successors :initform
               (make-array 0 :adjustable t :fill-pointer t)
               :reader root-node-successors)))

(defmethod add-successor ((self root-node) successor-node connector)
  (vector-push-extend
   (make-successor successor-node connector)
   (root-node-successors self))
  successor-node)

(defmethod accept-token ((self root-node) token)
  (map nil #'(lambda (successor)
               (funcall (successor-connector successor)
                        (successor-node successor)
                        token))
       (root-node-successors self)))

(defun make-root-node (class)
  (make-instance 'root-node :class class))

