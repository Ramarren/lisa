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

;;; File: reflect.lisp
;;; Description: Wrapper functions that provide the MOP functionality needed
;;; by LISA, hiding implementation details.

;;; $Id: reflect.lisp,v 1.7 2001/06/28 00:23:06 youngde Exp $

(in-package "LISA.REFLECT")

#+(or CLISP CMU)
(defun ensure-class (name &key (direct-superclasses '()))
  (eval `(defclass ,name ,direct-superclasses ())))

#+CLISP
(defun class-finalized-p (class)
  (declare (ignore class))
  (values t))

#+CLISP
(defun finalize-inheritance (class)
  (declare (ignore class))
  (values))

(defun class-slot-list (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (port:class-slot-list class))
