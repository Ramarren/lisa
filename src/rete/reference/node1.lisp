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

;;; $Id: node1.lisp,v 1.1 2002/08/28 15:54:13 youngde Exp $

(in-package "LISA")

(defclass node1 ()
  ((test :initarg :test
         :reader node1-test)
   (successors :initform
               (make-array 0 :adjustable t :fill-pointer t)
               :reader node1-successors)))

(defun add-successor (node1 connector)
  (vector-push-extend (node1-successors node1) connector))

(defun make-node1 (test)
  (make-instance 'node1 :test test))

