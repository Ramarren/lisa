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

;;; File: node1.lisp
;;; Description:

;;; $Id: node1.lisp,v 1.9 2002/08/30 16:54:00 youngde Exp $

(in-package "LISA")

(defclass node1 (shared-node)
  ((test :initarg :test
         :reader node1-test)))

(defmethod add-successor ((self node1) (new-node node1) connector)
  (with-slots ((successor-table successors)) self
    (let ((successor (gethash (node1-test new-node) successor-table)))
      (when (null successor)
        (setf successor
          (setf (gethash (node1-test new-node) successor-table)
            (make-successor new-node connector))))
      (successor-node successor))))

(defmethod add-successor ((self node1) (new-node t) connector)
  (setf (gethash new-node (shared-node-successors self))
    (make-successor new-node connector)))

(defmethod accept-token ((self node1) token)
  (if (funcall (node1-test self) token)
      (pass-token-to-successors self token)
    nil))

(defun make-node1 (test)
  (make-instance 'node1 :test test))

