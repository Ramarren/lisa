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

;;; File: dynamic-update.lisp
;;; Description: This file contains code supporting LISA's dynamic update
;;; capabilities.

;;; $Id: dynamic-update.lisp,v 1.2 2001/08/27 20:59:04 youngde Exp $

(in-package "LISA")

(defvar *in-dynamic-update* nil
  "Set whenever a dynamic update is in progress.")

(defun in-dynamic-update ()
  (values *in-dynamic-update*))

(defmacro with-gatekeeper ((node) &body body)
  (let ((pass-the-gate (gensym)))
    `(labels ((,pass-the-gate ()
                ,@body))
       (cond ((in-dynamic-update)
              (if (of-dynamic-update ,node)
                  (funcall #',pass-the-gate)
                (values nil)))
             (t (funcall #',pass-the-gate))))))

(defmacro with-dynamic-update ((paths) &body body)
  (let ((toggle-dynamic-update (gensym)))
    `(flet ((,toggle-dynamic-update (on)
              (dolist (path ,paths)
                (mapc #'(lambda (node)
                          (if on
                              (set-dynamic-update node)
                            (clear-dynamic-update node)))
                      path))))
       (let ((*in-dynamic-update* t))
         (funcall ,toggle-dynamic-update t)
         (progn ,@body)
         (funcall ,toggle-dynamic-update nil)))))
     
