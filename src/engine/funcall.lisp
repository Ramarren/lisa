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

;;; File: funcall.lisp
;;; Description: This class manages the mechanics of executing arbitrary Lisp
;;; code.

;;; $Id: funcall.lisp,v 1.1 2001/01/04 21:23:41 youngde Exp $

(in-package :lisa)

(defclass funcall ()
  ((forms :initarg :forms
          :reader get-forms)
   (bindings :accessor get-bindings))
  (:documentation
   "This class manages the mechanics of executing arbitrary Lisp code."))

(defmethod initialize-instance :after ((self funcall) &key bindings level)
  (flet ((save-bindingp (binding)
           (and (typep binding 'slot-binding)
                (<= (get-location binding) level))))
    (with-accessors ((bindings get-bindings)) self
      (maphash #'(lambda (key binding)
                   (when (save-bindingp binding)
                     (push binding bindings)))
               bindings))))

(defun make-funcall (forms bindings level)
  (make-instance 'funcall :forms forms :bindings bindings :level level))

