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

;;; File: node1-rtl.lisp
;;; Description: A node whose test always passes, but which transfers calls
;;; from the "right" side to the "left".

;;; $Id: node1-rtl.lisp,v 1.6 2000/11/28 14:37:30 youngde Exp $

(in-package :lisa)

(defclass node1-rtl (node1)
  ()
  (:documentation
   "A node whose test always passes, but which transfers calls from the
   'right' side to the 'left'."))

(defmethod call-node-right ((self node1-rtl) token)
  (call-next-method self token)
  (pass-along self token)
  (values t))

(defmethod pass-along ((self node1-rtl) token)
  (mapcar #'(lambda (node) (call-node-left node token))
          (get-successors self)))

(defmethod equals ((self node1-rtl) (obj node1-rtl))
  (values t))

(defun make-node1-rtl ()
  (make-instance 'node1-rtl))
