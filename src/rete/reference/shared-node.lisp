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

;;; File: shared-node.lisp
;;; Description:

;;; $Id: shared-node.lisp,v 1.1 2002/08/29 22:59:56 youngde Exp $

(in-package "LISA")

(defclass shared-node ()
  ((successors :initform (make-hash-table)
               :reader shared-node-successors)))

(defmethod add-successor ((self shared-node) successor-node connector)
  (with-slots ((successor-table successors)) self
    (unless (gethash successor-node successor-table)
      (setf (gethash successor-node successor-table) connector))
    successor-node))

(defmethod pass-token-to-successors ((self shared-node) token)
  (maphash #'(lambda (successor-node connector)
               (funcall connector successor-node token))
           (shared-node-successors self)))


