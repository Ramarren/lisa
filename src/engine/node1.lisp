;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: node1.lisp
;;; Description: Single-input node of the Rete pattern network. NODE1
;;; is intended to be abstract.

;;; $Id: node1.lisp,v 1.4 2000/11/09 19:26:04 youngde Exp $

(in-package :lisa)

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
