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

;;; File: network-node.lisp
;;; Description:

;;; $Id: network-node.lisp,v 1.1 2002/10/02 18:10:13 youngde Exp $

(in-package "LISA")

(defclass network-node ()
  ((refcnt :initform 0
           :accessor network-node-refcnt)))

(defmethod increment-use-count ((self network-node))
  (incf (network-node-refcnt self)))

(defmethod decrement-use-count ((self network-node))
  (decf (network-node-refcnt self)))

(defmethod node-use-count ((self network-node))
  (network-node-refcnt self))

(defun node-referenced-p (network-node)
  (plusp (node-use-count network-node)))
