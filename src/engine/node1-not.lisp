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

;;; File: node1-not.lisp
;;; Description: Simple composition class that negates the value of a NODE1.

;;; $Id: node1-not.lisp,v 1.2 2001/01/04 22:05:18 youngde Exp $

(in-package :lisa)

(defclass node1-not (node1)
  ((node :initarg :node
         :reader get-node))
  (:documentation
   "Simple composition class that negates the value of a NODE1."))

(defmethod call-node-right ((self node1-not) token)
  (not (call-node-right (get-node self) token)))

(defmethod equals ((self node1-not) (obj node1-not))
  (equals (get-node self) (get-node obj)))

(defmethod print-object ((self node1-not) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (print-unreadable-object (get-node self) strm)))

(defun make-node1-not (node1)
  (make-instance 'node1-not :node node1))


