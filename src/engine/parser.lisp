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
;;; Description: The LISA programming language parser.
;;;
;;; $Id: parser.lisp,v 1.40 2001/03/15 20:53:29 youngde Exp $

(in-package "LISA")

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
                          (make-parsed-pattern
                           :pattern (make-default-pattern (second p))
                           :type :negated))
                         ((variablep head)
                          (if (null binding)
                              (parse-pattern (first (rest p)) head)
                            (error "Parse error at ~S~%" p)))
                         (t
                          (make-parsed-pattern
                           :pattern (make-default-pattern p)
                           :binding binding
                           :type :generic)))
                 (error "Parse error at ~S~%" p)))))
    `(,(parse-pattern template nil))))

(defun parse-unordered-pattern (pattern)
  (labels ((parse-pattern-head ()
             (let ((head (first pattern)))
               (if (registered-classp head)
                   (values head)
                 (error "Pattern has no registered class: ~S" pattern))))
           (parse-slot (slot)
             (with-slot-components ((name field constraint) slot)
               (assert-conditions ((symbolp name)
                                   (slot-valuep field)
                                   (constraintp constraint)))
               `(,name ,field ,constraint)))
           (parse-pattern-body (body slots)
             (let ((slot (first body)))
               (cond ((consp slot)
                      (parse-pattern-body (rest body)
                                          (nconc slots
                                                  `(,(parse-slot slot)))))
                     ((null slot)
                      (values slots))
                     (t
                      (error "parse-unordered-pattern: parse error at ~S~%" body))))))
    `(,(parse-pattern-head)
      ,(parse-pattern-body (rest pattern) nil))))

(defun make-default-pattern (p)
  (parse-unordered-pattern p))

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
                    (error "NORMALIZE-SLOTS found a problem parsing ~S.~%" slots))))))
    `(list ,@(mapcar #'normalize slots))))

(defun parse-and-insert-fact (body)
  (let ((head (first body))
        (slots (rest body)))
    (cond ((symbolp head)
           (let ((class (find-registered-class head)))
             `(assert-fact (current-engine)
               (make-fact `,(get-name ,class)
                (,@(normalize-slots slots))))))
          (t
           (error "PARSE-AND-INSERT-FACT: parse error at ~S." body)))))

(defun parse-and-modify-fact (fact body)
  `(modify-fact (current-engine) ,fact (,@(normalize-slots body))))

(defun redefine-deftemplate (name body)
  (flet ((extract-slot (s)
           (cond ((or (not (consp s))
                      (not (eql (first s) 'slot))
                      (not (= (length s) 2)))
                  (error "Bad DEFTEMPLATE form for ~S: ~S" name s))
                 (t (second s)))))
    (create-class-template name (mapcar #'extract-slot body))))
