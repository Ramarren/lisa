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
;;;
;;; File: parser.lisp
;;; Description: The LISA programming language parser.
;;;
;;; $Id: parser.lisp,v 1.8 2000/11/09 21:22:42 youngde Exp $

(in-package :lisa)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defrule)))

(defun make-test-pattern (p)
  (list "test" p))

(defun make-negated-pattern (p)
  (list p))

(defmacro with-slot-components (((name field constraint) slot) &body body)
  `(destructuring-bind (,name ,field &optional ,constraint) ,slot
    ,@body))

(defmacro with-rule-components (((doc-string decls lhs rhs) rule-form) &body body)
  (let ((remains (gensym)))
    `(multiple-value-bind (,doc-string ,decls ,remains)
         (extract-rule-headers ,rule-form)
       (multiple-value-bind (,lhs ,rhs)
           (parse-rulebody ,remains)
         ,@body))))

(defun redefine-defrule (name body)
  (with-rule-components ((doc-string decls lhs rhs) body)
    (finalize-rule-definition
     (make-rule name :doc-string doc-string :source body)
     lhs rhs)))
    
(defun extract-rule-headers (body)
  (labels ((extract-headers (headers &key (doc nil))
             (let ((obj (first headers)))
               (cond ((stringp obj)
                      (if (null doc)
                          (extract-headers (rest headers) :doc obj)
                        (error "Parse error at ~S~%" headers)))
                     ((consp obj)
                      (let ((decl (first obj)))
                        (if (and (symbolp decl)
                                 (eq decl 'declare))
                            (values doc obj (rest headers))
                          (values doc nil headers))))
                     (t (values doc nil headers))))))
    (extract-headers body)))

(defun parse-rulebody (body)
  (labels ((parse-lhs (body &optional (patterns nil) (assign-to nil))
             (format t "parse-lhs: looking at ~S~%" body)
             (let ((pattern (first body)))
               (cond ((consp pattern)
                      (parse-lhs (rest body)
                                 (append patterns
                                         (make-rule-pattern pattern
                                                            assign-to))))
                     ((null pattern)
                      (values patterns))
                     ((variablep pattern)
                      (parse-lhs (rest body) patterns pattern))
                     (t (error "parse-rule-body: parsing error on LHS at ~S~%" patterns)))))
           (parse-rhs (actions)
             (values actions)))
    (multiple-value-bind (lhs remains)
        (find-before '=> body :test #'eq)
      (if (not (null remains))
          (values (parse-lhs lhs)
                  (parse-rhs (find-after '=> remains :test #'eq)))
        (error "parse-rulebody: rule structure unsound.")))))

(defun make-rule-pattern (template &optional (assign-to nil))
  (labels ((parse-pattern (p)
             (let ((head (first p)))
               (if (symbolp head)
                   (cond ((eq head 'test)
                          (make-test-pattern (rest p)))
                         ((eq head 'not)
                          (make-negated-pattern
                           (parse-pattern (first (rest p)))))
                         (t
                          (make-default-pattern p)))
                 (error "Parse error at ~S~%" p)))))
    `(,(parse-pattern template))))

(defun parse-ordered-pattern (pattern)
  (labels ((parse-fields (fields &optional (flist nil))
             (let ((field (first fields)))
               (cond ((null field)
                      (values flist))
                     ((literalp field)
                      (parse-fields (rest fields)
                                    (nconc flist `((,field)))))
                     ((variablep field)
                      (if (consp (second fields))
                          (parse-fields (rest (rest fields))
                                        (nconc flist
                                               `((,field ,(second fields)))))
                        (parse-fields (rest fields)
                                      (nconc flist `((,field))))))
                     (t
                      (error "parse-ordered-pattern: parse error for ~S~%"
                             pattern))))))
    `(,(canonicalize-pattern (first pattern)
                             (parse-fields (rest pattern))))))

(defun parse-unordered-pattern (pattern)
  (labels ((parse-slot (slot)
             (with-slot-components ((name field constraint) slot)
               (list name field constraint)))
           (parse-pattern-body (body &optional (slots nil))
             (let ((slot (first body)))
               (cond ((consp slot)
                      (parse-pattern-body (rest body)
                                          (append slots
                                                  `(,(parse-slot slot)))))
                     ((null slot)
                      (values slots))
                     (t
                      (error "parse-unordered-pattern: parse error at ~S~%" body))))))
    (list (first pattern)
          (parse-pattern-body (rest pattern)))))

(defun internalize-class (name slots)
  "Creates an internal, LISA-specific class representing ordered
  patterns that have been canonicalized to their unordered
  equivalents."
  (flet ((compose-slots (slot-list)
           (mapcar #'(lambda (slot)
                       `(,(first slot) :initform nil))
                   slot-list)))
    (make-internal-class name (compose-slots slots))))

(defun canonicalize-pattern (head body)
  (labels ((make-slot-id (id)
             (intern
              (make-symbol
               (format nil "__SLOT-~D" id))))
           (make-slot-list (body slot-id &optional (slots nil))
             (if (null body)
                 (values slots)
               (make-slot-list (rest body)
                               (1+ slot-id)
                               (nconc slots
                                      `(,(cons (make-slot-id slot-id)
                                               (first body)))))))
           (finalize-pattern (head slots)
             (internalize-class head slots)
             (values `(,head ,slots))))
    (finalize-pattern head (make-slot-list body 0))))

(defun make-default-pattern (p)
  (if (unordered-patternp p)
      (parse-unordered-pattern p)
    (parse-ordered-pattern p)))

