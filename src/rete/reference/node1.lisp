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

;;; $Id: node1.lisp,v 1.7 2002/08/30 01:41:20 youngde Exp $

(in-package "LISA")

(defclass node1 ()
  ((test :initarg :test
         :reader node1-test)
   (successor :initform nil
              :reader node1-successor)))

(defun pass-token-to-successor (node1 token)
  (let ((successor (node1-successor node1)))
    (funcall (successor-connector successor)
             (successor-node successor)
             token)))

(defmethod add-successor ((self node1) successor-node connector)
  (setf (slot-value self 'successor)
    (make-successor successor-node connector)))

(defmethod accept-token ((self node1) token)
  (if (funcall (node1-test self) token)
      (pass-token-to-successor self token)
    nil))

(defun make-node1 (test)
  (make-instance 'node1 :test test))

