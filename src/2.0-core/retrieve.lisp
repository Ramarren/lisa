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
;;; Description:

;;; $Id: retrieve.lisp,v 1.4 2002/12/20 00:13:18 youngde Exp $

(in-package "LISA")

(defvar *query-result*
    '()
  "Holds the results of query firings.")

(defun run-query (query-rule)
  "Runs a query (RULE instance), and returns both the value of *QUERY-RESULT*
  and the query name itself."
  (let ((*query-result* (list)))
    (assert (query-fact))
    (run)
    *query-result*))

#+ignore
(defun run-query (rule)
  rule)

#+ignore
(defmacro defquery (name &body body)
  (print name) 
  (print body)
  nil)

(defmacro defquery (name &body body)
  "Defines a new query identified by the symbol NAME."
  `(define-rule ,name ',body))

;;; Queries fired by RETRIEVE collect their results in the special variable
;;; *QUERY-RESULT*. As an example, one firing of this query, 
;;;
;;;   (retrieve (?x ?y) 
;;;     (?x (rocky (name ?name)))
;;;     (?y (hobbit (name ?name))))
;;;
;;; will produce a result similar to,
;;;
;;; (((?X . #<ROCKY @ #x7147b70a>) (?Y . #<HOBBIT @ #x7147b722>)))

(defmacro retrieve ((&rest varlist) &body body)
  (flet ((make-query-binding (var)
           `(cons ',var (find-instance-of-fact ,var))))
    (let ((query-name (gensym))
          (query (gensym)))
      `(with-inference-engine ((make-query-engine (inference-engine)))
         (let* ((,query-name (gensym))
                (,query
                 (defquery ',query-name
                     (query-fact)
                   ,@body
                   =>
                   (push (list ,@(mapcar #'make-query-binding varlist))
                         *query-result*))))
           (run-query ,query))))))

(defmacro with-simple-query ((var value) query &body body)
  "For each variable/instance pair in a query result, invoke BODY with VAR
  bound to the query variable and VALUE bound to the instance."
  (let ((result (gensym)))
    `(let ((,result ,query))
       (dolist (match ,result)
         (dolist (binding match)
           (let ((,var (car binding))
                 (,value (cdr binding)))
             ,@body))))))
