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

;;; $Id: shared-node.lisp,v 1.4 2002/08/30 16:54:00 youngde Exp $

(in-package "LISA")

(defclass shared-node ()
  ((successors :initform (make-hash-table :test #'equal)
               :reader shared-node-successors)))

(defmethod pass-token-to-successors ((self shared-node) token)
  (maphash #'(lambda (key successor)
               (declare (ignore key))
               (funcall (successor-connector successor)
                        (successor-node successor)
                        token))
           (shared-node-successors self)))
