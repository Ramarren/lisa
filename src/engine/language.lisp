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
;;; File: language.lisp
;;; Description: Code that implements the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.8 2000/10/20 18:07:50 youngde Exp $

(in-package "LISA")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defrule)))

(defun make-test-pattern (p)
  (list "test" p))

(defun make-negated-pattern (p)
  (list p))

(defun make-assignment-pattern (p)
  (list p))

(defun make-pattern (p)
  (list p))

(defmacro defrule (name &body body)
  "Creates or redefines a rule in the network."
  `(redefine-defrule ',name ',body))

(defmacro variablep (sym)
  `(and (symbolp ,sym)
    (eq (elt (symbol-name ,sym) 0) #\?)))

(defmacro identifierp (sym)
  `(or
    (symbolp ,sym)
    (numberp ,sym)
    (stringp ,sym)))

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
    (let ((rule (make-defrule name :doc-string doc-string :source body)))
      (mapc #'(lambda (p)
                (add-pattern rule p))
            lhs)
      (set-actions rule (compile-function rhs))
      (values rule))))
    
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
             (values actions))
           (overall-structure-ok (body)
             (< (position 'when body :test #'eq :key #'car)
                (position 'then body :test #'eq :key #'car))))
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
                          (parse-fact p)))
                 (error "Parse error at ~S~%" p)))))
    (if (null assign-to)
        (make-pattern (parse-pattern template))
      (make-assignment-pattern (parse-pattern template)))))
        
(defun parse-fact (fact)
  (labels ((parse-slot (slot)
             (with-slot-components ((name field constraint) slot)
               (list name field constraint)))
           (parse-fact-body (body &optional (slots nil))
             (let ((slot (first body)))
               (cond ((consp slot)
                      (parse-fact-body (rest body)
                                       (append slots
                                               `(,(parse-slot slot)))))
                     ((null slot)
                      (values slots))
                     (t
                      (error "parse-fact: parse error at ~S~%" body))))))
    (list (first fact)
          (parse-fact-body (rest fact)))))
