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
;;; Description: Single-input node of the Rete pattern network. NODE1
;;; is intended to be abstract.

;;; $Id: node1.lisp,v 1.8 2001/03/15 16:00:30 youngde Exp $

(in-package "LISA")

(defclass node1 (node)
  ()
  (:documentation
   "Single-input node of the Rete pattern network. NODE1 is intended
   to be abstract."))

(defmethod call-node-right ((self node1) (token clear-token))
  (pass-along self token)
  (values t))

(defmethod call-node-right ((self node1) (token token))
  (values nil))

(defmethod pass-along ((self node1) token)
  (mapcar #'(lambda (n) (call-node-right n token))
        (get-successors self)))

(defmethod equals ((self node1) obj)
  (declare (ignore obj))
  (values nil))
