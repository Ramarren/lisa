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

;;; File: context.lisp
;;; Description:

;;; $Id: context.lisp,v 1.1 2002/11/18 20:14:50 youngde Exp $

(in-package "LISA")

(defclass context ()
  ((name :initarg :name
         :reader context-name)
   (strategy :initarg :strategy
             :reader context-strategy)))

(defun make-context (name &key (strategy nil))
  (make-instance 'context
    :name name
    :strategy (if (null strategy)
                  (make-breadth-first-strategy)
                strategy)))
