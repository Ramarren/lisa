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
;;; Description: Macros and functions implementing LISA's initial query
;;; language implementation.

;;; $Id: retrieve.lisp,v 1.12 2002/04/08 02:19:54 youngde Exp $

(in-package "LISA")

(defvar *query-result* nil)

(deftemplate query-fact ()
  (slot name))

(defun run-query (query)
  (with-inference-engine ((current-engine))
    (let* ((?name (get-name query))
           (*query-result* '())
           (fact (assert (query-fact (name ?name)))))
      (run)
      (retract fact)
      (values *query-result* ?name))))

(defun define-query (name body)
  (let ((rule (define-rule name body)))
    (add-rule (current-engine) rule)
    (values rule)))

(defmacro defquery (name &body body)
  `(define-query ,name ',body))

(defmacro retrieve ((&rest varlist) &body body)
  (flet ((make-query-binding (var)
           `(cons ',var (instance-of-shadow-fact ,var))))
    (let ((query-name (gentemp))
          (hash (gensym))
          (query (gensym)))
      `(let* ((,hash (sxhash (normalize-query ',body)))
              (,query (find-query ,hash)))
         (when (null ,query)
           (setf ,query
             (defquery ',query-name
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

(defun forget-query (name)
  (flet ((remove-query (key name)
           (remhash key *query-map*)
           (undefrule name (current-engine))))
    (block found
      (with-hash-table-iterator (next-item *query-map*)
        (multiple-value-bind (foundp key value) (next-item)
          (when (and foundp (eq value name))
            (return-from found
              (remove-query key value))))))))

(defun remember-query (hash query)
  (setf (gethash hash *query-map*) (get-name query)))

(defmethod clear-engine :after ((self rete))
  (forget-all-queries))

(defun find-query (hash)
  (let ((query-name (gethash hash *query-map*)))
    (if (not (null query-name))
        (progn
          (format t "Good. Found query in cache.~%")
          (find-rule (current-engine) query-name))
      (values nil))))

(defun normalize-query (body)
  (let ((varlist '())
        (index 0))
    (labels ((map-variable (var)
               (let ((item (cdr (assoc var varlist))))
                 (when (null item)
                   (setf item
                     (make-symbol (format nil "?_~D" (incf index))))
                   (setf varlist (acons var item varlist)))
                 (values item)))
             (transform (obj)
               (cond ((variablep obj)
                      (prin1-to-string (map-variable obj)))
                     ((stringp obj) obj)
                     (t (prin1-to-string obj)))))
      (mapcar #'(lambda (pattern)
                  (sort (mapcar #'transform (flatten pattern))
                        #'string<))
              body))))
             
