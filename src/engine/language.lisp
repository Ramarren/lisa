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
;;; Description: Macros that implement the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.6 2000/10/19 20:28:04 youngde Exp $

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

(defmacro defrule (name &body body)
  "Creates or redefines a rule in the network."
  `(redefine-defrule ',name ',body))

(defun redefine-defrule (name body)
  (multiple-value-bind (doc-string decls lhs rhs)
      (evaluate-defrule body)
    (let ((rule (make-defrule name :doc-string doc-string)))
      (mapc #'(lambda (p)
                (add-pattern rule p))
            lhs)
      (set-actions rule (compile-function rhs))
      (values rule))))
      
(defun evaluate-defrule (body)
  (multiple-value-bind (doc-string decls remains)
      (extract-rule-headers body)
    (multiple-value-bind (lhs rhs)
        (parse-rulebody remains)
      (values doc-string decls lhs rhs))))

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
             (let ((pattern (first body)))
               (cond ((consp pattern)
                      (parse-lhs (rest body)
                                 (append patterns
                                         (make-rule-pattern pattern
                                         assign-to))))
                     ((symbolp pattern)
                      (cond ((eq pattern '=>)
                             (values patterns (rest body)))
                            ((variablep pattern)
                             (parse-lhs
                              (rest body) patterns pattern))
                            (t (error "Parsing error on LHS at ~S~%" body))))
                     (t (error "Parsing error on LHS at ~S~%" patterns)))))
           (parse-rhs (actions)
             (values actions)))
    (multiple-value-bind (patterns remains)
        (parse-lhs body)
      (values patterns (parse-rhs remains)))))

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
