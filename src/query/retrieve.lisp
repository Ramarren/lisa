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

;;; $Id: retrieve.lisp,v 1.8 2002/04/04 03:39:24 youngde Exp $

(in-package "LISA")

(defvar *query-result* nil)

(deftemplate query-fact ()
  (slot name))

(defun run-query (query)
  (with-inference-engine ((current-engine))
    (let ((?name (get-name query))
          (*query-result* '()))
      (assert (query-fact (name ?name)))
      (run)
      (values *query-result*))))

#+ignore
(defun run-query (query))

(defun define-query (name body)
  (format t "name ~S, body ~S~%" name body)
  (let ((rule (define-rule name body)))
    (add-rule (current-engine) rule)
    (values rule)))

(defmacro defquery (name &body body)
  (let ((rule-name (gensym)))
    `(let ((,rule-name
            (if (consp ',name) ,name ',name)))
       (define-query ',rule-name ',body))))

(defmacro retrieve ((&rest varlist) &body body)
  (flet ((make-query-binding (var)
           `(cons ',var (instance-of-shadow-fact ,var))))
    (let ((query (gensym))
          (query-name (gensym))
          (hash (gensym)))
      `(let* ((,hash (normalize-query ',body))
              (,query (find-query ,hash)))
         (when (null ,query)
;;;             (let ((,query-name (gensym)))
           (setf ,query
             (defquery query-name
                 (query-fact (name ,query-name))
               ,@body
               =>
               (push (list ,@(mapcar #'make-query-binding varlist))
                     *query-result*)))
               (remember-query ,hash ,query))
         (run-query ,query)))))

(defvar *query-map*
    (make-hash-table))

(defun forget-all-queries ()
  (clrhash *query-map*))

(defun remember-query (hash query)
  (setf (gethash hash *query-map*) (get-name query)))

(defun find-query (hash)
  (let ((query-name (gethash hash *query-map*)))
    (if (not (null query-name))
        (find-rule (current-engine) query-name)
      (values nil))))

(defun normalize-query (body)
  (let ((varlist '())
        (index 0))
    (flet ((map-variable (var)
             (let ((item (cdr (assoc var varlist))))
               (when (null item)
                 (setf item
                   (make-symbol (format nil "?_~D" (incf index))))
                 (setf varlist (acons var item varlist)))
               (values item))))
      (mapcar #'(lambda (obj)
                  (if (variablep obj) (map-variable obj) obj))
              (flatten body)))))
             
