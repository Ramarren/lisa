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
;;; $Id: parser.lisp,v 1.84 2002/07/29 17:24:55 youngde Exp $

(in-package "LISA")

(defconstant *rule-separator* '=>)

(defmacro with-rule-components (((doc-string lhs rhs) rule-form) &body body)
  (let ((remains (gensym)))
    `(multiple-value-bind (,doc-string ,remains)
         (extract-rule-headers ,rule-form)
       (multiple-value-bind (,lhs ,rhs)
           (parse-rulebody ,remains)
         ,@body))))

(defun define-rule (name body &optional (salience 0) (module nil))
  (handler-case
      (with-rule-components ((doc-string lhs rhs) body)
        (let ((rule (make-rule name (current-engine)
                               :doc-string doc-string
                               :salience salience
                               :module module
                               :source body)))
          (finalize-rule-definition rule lhs rhs)
          (values rule)))
    (syntactical-error (condition)
      (rule-structure-error name condition))))
  
(defun redefine-defrule (name body &key (salience 0) (module nil))
  (add-rule (current-engine) (define-rule name body salience module)))

(defun extract-rule-headers (body)
  (if (stringp (first body))
      (values (first body) (rest body))
    (values nil body)))

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
                      (pattern-error pattern "Incorrect structure.")))))
           (parse-rhs (actions)
             (values actions)))
    (multiple-value-bind (lhs remains)
        (utils:find-before *rule-separator* body :test #'eq)
      (if (not (null remains))
          (values (parse-lhs lhs nil)
                  (parse-rhs (utils:find-after *rule-separator* remains :test #'eq)))
        (parsing-error "Missing rule separator.")))))

(defun make-rule-pattern (template)
  (labels ((parse-pattern (p binding)
             (let ((head (first p)))
               (if (symbolp head)
                   (cond ((eq head 'test)
                          (make-parsed-pattern
                           :pattern (parse-test-pattern p)
                           :type :test))
                         ((eq head 'not)
                          (make-parsed-pattern
                           :pattern `(,@(parse-default-pattern (second p)))
                           :type :negated))
                         ((variablep head)
                          (cl:assert (null binding) nil
                            "Too many pattern variables: ~S" head)
                          (parse-pattern (first (rest p)) head))
                         (t
                          (make-parsed-pattern
                           :pattern `(,@(parse-default-pattern p))
                           :binding binding
                           :type (if binding :bound :generic))))
                 (pattern-error
                  template "Patterns must begin with a symbol.")))))
    `(,(parse-pattern template nil))))

(defun parse-test-pattern (pattern)
  (let ((form (rest pattern)))
    (if (and (listp form)
             (= (length form) 1))
        (values (first form))
    (pattern-error
     pattern "The body of a TEST CE must be a single Lisp form"))))

(defun parse-default-pattern (pattern)
  (let* ((head (first pattern))
         (meta (find-meta-fact head nil)))
    (when (null meta)
      (pattern-error
       pattern "This pattern is not supported by any known class."))
    (labels ((parse-slot (slot)
               (let ((name (first slot))
                     (field (second slot))
                     (constraint (third slot)))
                 (cond ((and (symbolp name)
                             (slot-valuep field)
                             (constraintp constraint))
                        (cl:assert (has-meta-slot-p meta name) nil
                          "This slot has no meta data: ~S." slot)
                        `(,name ,field ,constraint))
                       (t
                        (cl:assert nil nil
                            "There are type problems with this slot: ~S." slot)))))
             (parse-pattern-body (body slots)
               (let ((slot (first body)))
                 (cond ((consp slot)
                        (parse-pattern-body (rest body)
                                            (nconc slots
                                                   `(,(parse-slot slot)))))
                       ((null slot)
                        slots)
                       (t
                        (pattern-error
                         pattern "Found one or more structural problems."))))))
    `(,head ,(parse-pattern-body (rest pattern) nil)))))

(defun normalize-slots (slots)
  (flet ((normalize-slot (slot)
           (let ((slot-name (first slot))
                 (slot-value (second slot)))
             (cond ((symbolp slot-name)
                    (if (quotablep slot-value)
                        ``(,',slot-name ,',slot-value)
                      ``(,',slot-name ,,slot-value)))
                   (t
                    (parsing-error
                     "There's a type problem in this slot: ~S." slot))))))
    `(list ,@(mapcar #'normalize-slot slots))))

(defun canonicalize-slot-names (meta-class slots)
  (mapcar #'(lambda (slot)
              `(,(find-meta-slot meta-class (first slot))
                ,(second slot)))
          slots))

(defun parse-and-insert-fact (body)
  (let ((head (first body))
        (slots (rest body)))
    (cond ((symbolp head)
           (let ((meta-class (gensym)))
             `(let ((,meta-class (find-meta-fact ',head)))
               (assert-fact (current-engine)
                (make-fact ',head
                 (canonicalize-slot-names
                  ,meta-class (,@(normalize-slots slots))))))))
          (t
           (cl:assert nil nil
             "A fact must begin with a symbol: ~S." head)))))

(defun parse-and-modify-fact (fact body)
  (flet ((generate-modify ()
           (let ((meta-class (gensym)))
             `(let ((,meta-class (find-meta-fact (fact-name ,fact))))
               (modify-fact (current-engine) ,fact
                (canonicalize-slot-names ,meta-class
                 (,@(normalize-slots body))))))))
    (handler-case
        (generate-modify)
      (lisa-error (condition)
        (command-structure-error 'modify-fact condition)))))

(defun create-template-class-slots (class-name slot-list)
  (labels ((determine-default (default-form)
             (cl:assert (and (consp default-form)
                             (eq (first default-form) 'default)
                             (= (length default-form) 2)) ()
                        "Bogus DEFAULT operator (~S) in DEFTEMPLATE form."
                        default-form)
             (second default-form))
           (build-one-slot (template)
             (destructuring-bind (keyword slot-name &optional default)
                 template
               (cl:assert (eq keyword 'slot) ()
                          "Unrecognized keyword (~S) in DEFTEMPLATE form."
                          keyword)
               `(,slot-name
                 :initarg ,(intern (symbol-name slot-name) 'keyword)
                 :initform
                 ,(if (null default) nil (determine-default default))
                 :reader 
                 ,(intern (format nil "~S-~S" class-name slot-name))))))
    (mapcar #'build-one-slot slot-list)))

(defun redefine-deftemplate (class-name body)
  (let ((class (gensym)))
    `(let ((,class
            (defclass ,class-name (inference-engine-object)
              ,@(list (create-template-class-slots class-name body)))))
       (register-template ',class-name ,class)
       ,class)))

(defun parse-and-insert-instance (instance)
  (assert-fact
   (current-engine)
   (make-fact-from-instance (find-symbolic-name instance) instance)))

(defun parse-and-retract-instance (instance)
  (let ((engine (current-engine)))
    (retract-fact engine (find-fact-using-instance engine instance))))

(defun show-deffacts (deffact)
  (format t "~S~%" deffact)
  (values deffact))

(defun parse-and-insert-deffacts (name body)
  `(let ((deffacts '()))
     (dolist (fact ',body)
       (let* ((head (first fact))
              (meta-class (find-meta-fact head)))
         (push (make-fact 
                head (canonicalize-slot-names meta-class (rest fact)))
               deffacts)))
     (add-autofact (current-engine)
                   (make-deffacts ',name (nreverse deffacts)))))
       
