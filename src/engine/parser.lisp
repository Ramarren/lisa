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
;;; $Id: parser.lisp,v 1.21 2000/12/10 03:26:01 youngde Exp $

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
    (let ((rule (make-rule name (current-engine)
                           :doc-string doc-string :source body)))
      (finalize-rule-definition rule lhs rhs)
      (add-rule (current-engine) rule))))

(defun extract-rule-headers (body)
  (labels ((extract-headers (headers doc)
             (let ((obj (first headers)))
               (cond ((stringp obj)
                      (if (null doc)
                          (extract-headers (rest headers) obj)
                        (error "Parse error at ~S~%" headers)))
                     ((consp obj)
                      (let ((decl (first obj)))
                        (if (and (symbolp decl)
                                 (eq decl 'declare))
                            (values doc obj (rest headers))
                          (values doc nil headers))))
                     (t (values doc nil headers))))))
    (extract-headers body nil)))

(defun parse-rulebody (body)
  (labels ((parse-lhs (body patterns)
             (format t "parse-lhs: looking at ~S~%" body)
             (let ((pattern (first body)))
               (cond ((consp pattern)
                      (parse-lhs (rest body)
                                 (append patterns
                                         (make-rule-pattern pattern))))
                     ((null pattern)
                      (values patterns))
                     (t (error "parse-rule-body: parsing error on LHS at ~S~%" patterns)))))
           (parse-rhs (actions)
             (values actions)))
    (multiple-value-bind (lhs remains)
        (find-before '=> body :test #'eq)
      (if (not (null remains))
          (values (parse-lhs lhs nil)
                  (parse-rhs (find-after '=> remains :test #'eq)))
        (error "parse-rulebody: rule structure unsound.")))))

(defun make-rule-pattern (template)
  (labels ((parse-pattern (p binding)
             (let ((head (first p)))
               (if (symbolp head)
                   (cond ((eq head 'test)
                          (make-test-pattern (rest p)))
                         ((eq head 'not)
                          (make-negated-pattern
                           (parse-pattern (first (rest p)) binding)))
                         ((variablep head)
                          (if (null binding)
                              (parse-pattern (first (rest p)) head)
                            (error "Parse error at ~S~%" p)))
                         (t
                          (make-parsed-pattern
                           :pattern (make-default-pattern p)
                           :binding binding)))
                 (error "Parse error at ~S~%" p)))))
    `(,(parse-pattern template nil))))

(defun parse-unordered-pattern (pattern)
  (labels ((parse-slot (slot)
             (with-slot-components ((name field constraint) slot)
               (list name field constraint)))
           (parse-pattern-body (body slots)
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
          (parse-pattern-body (rest pattern) nil))))

(defun make-default-pattern (p)
  (parse-unordered-pattern p))

(defun normalize-slots (&rest args)
  (labels ((compose-slots (pairs slot-name slots)
             (cond ((null pairs)
                    (values slots))
                   ((null slot-name)
                    (compose-slots (rest pairs) (first pairs) slots))
                   (t
                    (compose-slots (rest pairs) nil
                                   (nconc slots
                                          `((,slot-name 
                                             ,(first pairs)))))))))
    (compose-slots args nil nil)))

(defun expand-slot-variables (slots)
  (labels ((validate-slot-structure (slot)
             (if (consp slot)
                 (let ((name (first slot))
                       (value (second slot)))
                   (cond ((and (symbolp name)
                               (or (literalp value)
                                   (variablep value)))
                          (values name value))
                         (t
                          (error "EXPAND-SLOT-VARIABLES: malformed slot ~S." slot))))
               (error "EXPAND-SLOT-VARIABLES: parse error at ~S." slot)))
           (expand-slots (slots bindings)
             (let ((slot (first slots)))
               (if (null slot)
                   (values bindings)
                 (multiple-value-bind (name value)
                     (validate-slot-structure slot)
                   (expand-slots (rest slots)
                                 (nconc bindings `(',name ,value))))))))
    (expand-slots slots nil)))

(defun parse-and-insert-fact (body)
  (let ((head (first body))
        (slots (rest body)))
    (cond ((symbolp head)
           (let ((class (find-imported-class head nil)))
             (if (null class)
                 (error "No class object found for ~S." head)
               `(assert-fact (current-engine)
                             (make-fact ,class
                                        (normalize-slots
                                         ,@(expand-slot-variables slots)))))))
          (t
           (error "PARSE-AND-INSERT-FACT: parse error at ~S." body)))))

(defun parse-and-modify-fact (fact body)
  `(format t "modify fact: ~S ~S~%" ,fact
           (normalize-slots ,@(expand-slot-variables body))))
