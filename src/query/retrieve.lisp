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

;;; File: retrieve.lisp
;;; Description: Macros and functions implementing LISA's early attempt at a
;;; query language.

;;; $Id: retrieve.lisp,v 1.4 2002/01/10 15:40:42 youngde Exp $

(in-package "LISA")

(defvar *query-result* nil)

(defun define-and-run-query (name body)
  (let ((*query-result* '())
        (query-engine (make-rete)))
    (setf (get-facts query-engine) (get-facts (current-engine)))
    (add-rule query-engine (define-rule name body))
    (run-engine query-engine)
    (values *query-result*)))

(defmacro defquery (name &body body)
  (let ((rule-name (gensym)))
    `(let ((,rule-name
            (if (consp ',name) ,name ',name)))
       (define-and-run-query ,rule-name ',body))))

(defmacro retrieve ((&rest varlist) &body body)
  (flet ((make-query-binding (var)
           `(cons ',var (instance-of-shadow-fact ,var))))
    `(defquery (gensym)
       ,@body
       =>
       (push (list ,@(mapcar #'make-query-binding varlist)) *query-result*))))
