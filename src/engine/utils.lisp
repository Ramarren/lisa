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

;;; File: utils.lisp
;;; Description: Various utilities useful to the inference engine.

;;; $Id: utils.lisp,v 1.8 2001/03/15 16:00:31 youngde Exp $

(in-package "LISA")

#|
(defun make-internal-class (name slots)
  (flet ((validate-class (class)
           (when (not (= (length slots)
                         (length (clos:class-direct-slots class))))
             (error "New definition of class ~S inconsistent with existing instance." name))))
    (let ((class (find-class name nil)))
      (when (null class)
        (setf class
          (eval `(defclass ,name (lisa-kb-class)
                   (,@slots))))
        (clos:finalize-inheritance class)
        (import-class (class-name class) class))
      (validate-class class)
      (values class))))

(defun has-superclass (class super)
  (member super (clos:class-precedence-list class)))

(defun is-lisa-class (class)
  (has-superclass class (find-class 'lisa-kb-class)))
|#
