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

;;; File: parser.lisp
;;; Description: The LISA programming language parser. Basic parsing and
;;; validation is done here, but it isn't comprehensive. Additional parsing
;;; work (and error discovery) for some constructs (such as defrule, assert,
;;; modify) is performed elsewhere as these constructs undergo additional
;;; transformations.
;;;
;;; $Id: parser.lisp,v 1.46 2001/03/30 20:25:44 youngde Exp $

(in-package "LISA")

(defconstant *rule-separator* '=>)

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
  (flet ((redefine-rule ()
           (with-rule-components ((doc-string decls lhs rhs) body)
             (let ((rule (make-rule name (current-engine)
                                    :doc-string doc-string :source body)))
               (finalize-rule-definition rule lhs rhs)
               (add-rule (current-engine) rule)))))
    (handler-case
        (redefine-rule)
      (syntactical-error (condition)
        (rule-structure-error name condition)))))

(defun extract-rule-headers (body)
  (labels ((extract-headers (headers doc)
             (let ((obj (first headers)))
               (cond ((stringp obj)
                      (if (null doc)
                          (extract-headers (rest headers) obj)
                        (parsing-error
                         "Too many documentation strings: ~S" obj)))
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
             (let ((pattern (first body)))
               (cond ((consp pattern)
                      (parse-lhs (rest body)
                                 (nconc patterns
                                        (make-rule-pattern pattern))))
                     ((null pattern)
                      (values patterns))
                     (t
                      (parsing-error
                       "There was trouble while parsing the LHS: ~S" pattern)))))
           (parse-rhs (actions)
             (values actions)))
    (multiple-value-bind (lhs remains)
        (find-before *rule-separator* body :test #'eq)
      (if (not (null remains))
          (values (parse-lhs lhs nil)
                  (parse-rhs (find-after *rule-separator remains :test #'eq)))
        (parsing-error "Missing rule separator.")))))

(defun make-rule-pattern (template)
  (labels ((parse-pattern (p binding)
             (let ((head (first p)))
               (if (symbolp head)
                   (cond ((eq head 'test)
                          (make-test-pattern (rest p)))
                         ((eq head 'not)
                          (make-parsed-pattern
                           :pattern (make-default-pattern (second p))
                           :type :negated))
                         ((variablep head)
                          (if (null binding)
                              (parse-pattern (first (rest p)) head)
                            (parsing-error
                             "Too many pattern variables: ~S" head)))
                         (t
                          (make-parsed-pattern
                           :pattern (make-default-pattern p)
                           :binding binding
                           :type :generic)))
                 (parsing-error
                  "Patterns must begin with a symbol: ~S" template)))))
    `(,(parse-pattern template nil))))

(defun parse-default-pattern (pattern)
  (labels ((parse-pattern-head ()
             (let ((head (first pattern)))
               (if (has-meta-classp head)
                   (values head)
                 (parsing-error "This pattern has no meta class: ~S" head))))
           (parse-slot (slot)
             (with-slot-components ((name field constraint) slot)
               (cond ((and (symbolp name)
                           (slot-valuep field)
                           (constraintp constraint))
                      `(,name ,field ,constraint))
                     (t
                      (parsing-error
                       "In pattern ~S there are type problems with this slot: ~S" slot)))))
           (parse-pattern-body (body slots)
             (let ((slot (first body)))
               (cond ((consp slot)
                      (parse-pattern-body (rest body)
                                          (nconc slots
                                                  `(,(parse-slot slot)))))
                     ((null slot)
                      (values slots))
                     (t
                      (parsing-error
                       "This pattern has structural problems: ~S" pattern))))))
    `(,(parse-pattern-head)
      ,(parse-pattern-body (rest pattern) nil))))

(defun make-default-pattern (p)
  (parse-default-pattern p))

(defun normalize-slots (slots)
  (flet ((normalize (slot)
           (let ((slot-name (first slot))
                 (slot-value (second slot)))
             (cond ((and (symbolp slot-name)
                         (or (literalp slot-value)
                             (variablep slot-value)))
                    (if (quotablep slot-value)
                        ``(,',slot-name ,',slot-value)
                      ``(,',slot-name ,,slot-value)))
                   (t
                    (parsing-error
                     "There's a type problem in this slot: ~S" slot))))))
    `(list ,@(mapcar #'normalize slots))))

(defun canonicalize-slot-names (meta-class slots)
  (mapcar #'(lambda (slot)
              `(,(find-meta-slot meta-class (first slot))
                ,(second slot)))
          slots))

(defun parse-and-insert-fact (body)
  (let ((head (first body))
        (slots (rest body)))
    (cond ((symbolp head)
           (let ((class (find-meta-class head)))
             `(assert-fact (current-engine)
               (make-fact ',(get-name class)
                (canonicalize-slot-names ,class
                 (,@(normalize-slots slots)))))))
          (t
           (parsing-error "A fact must begin with a symbol: ~S" head)))))

(defun parse-and-modify-fact (fact body)
  `(modify-fact (current-engine) ,fact
    (canonicalize-slot-names (find-meta-class (fact-name ,fact))
     (,@(normalize-slots body)))))

(defun redefine-deftemplate (name body)
  (flet ((extract-slot (slot)
           (cond ((or (not (consp slot))
                      (not (eql (first slot) 'slot))
                      (not (= (length slot) 2)))
                  (parsing-error
                   "This slot has a structural problem: ~S" slot))
                 (t (second slot)))))
    (create-class-template name (mapcar #'extract-slot body))))
