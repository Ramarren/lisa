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
;;; $Id: parser.lisp,v 1.9 2002/08/26 01:36:25 youngde Exp $

(in-package "LISA")

(defconstant *rule-separator* '=>)

(defvar *binding-table* nil)

(defun find-or-set-slot-binding (var slot-name location)
  (let ((binding
         (gethash var *binding-table*)))
    (when (null binding)
      (setf binding
        (setf (gethash var *binding-table*)
          (list var slot-name location))))
    binding))

(defun find-slot-binding (var)
  (let ((binding (gethash var *binding-table*)))
    (cl:assert (not (null binding)) nil
      "There's no slot binding for variable ~S" var)
    binding))

(defmacro push-slot-binding (var name address list)
  `(pushnew (find-or-set-slot-binding ,var ,name ,address) ,list))

(defmacro push-test-bindings (test-form list)
  `(loop for obj in (utils:flatten ,test-form)
       if (variablep obj)
       collect (find-slot-binding obj)))

(defmacro push-binding (var list)
  `(pushnew (find-slot-binding ,var) ,list))

(defmacro with-rule-components (((doc-string lhs rhs) rule-form) &body body)
  (let ((remains (gensym)))
    `(multiple-value-bind (,doc-string ,remains)
         (extract-rule-headers ,rule-form)
       (multiple-value-bind (,lhs ,rhs)
           (let ((*binding-table* (make-hash-table)))
             (parse-rulebody ,remains))
         ,@body))))

(defun define-rule (name body &optional (salience 0) (module nil))
  (with-rule-components ((doc-string lhs rhs) body)
    (format t "LHS: ~S~%" lhs)
    (format t "RHS: ~S~%" rhs)
    (let ((rule (make-rule name (current-engine)
                           :doc-string doc-string
                           :salience salience
                           :module module)))
      (finalize-rule-definition rule lhs rhs)
      rule)))

(defun redefine-defrule (name body &key (salience 0) (module nil))
  (add-rule (current-engine) (define-rule name body salience module)))

(defun extract-rule-headers (body)
  (if (stringp (first body))
      (values (first body) (rest body))
    (values nil body)))

(defun parse-rulebody (body)
  (let ((location -1))
    (labels ((parse-lhs (body patterns)
               (let ((pattern (first body)))
                 (cl:assert (listp pattern) nil
                   "This pattern is malformed: ~S" pattern)
                 (if (consp pattern)
                     (parse-lhs 
                      (rest body)
                      (push
                       (make-rule-pattern pattern (incf location)) patterns))
                   (nreverse patterns))))
             (parse-rhs (actions) actions))
      (multiple-value-bind (lhs remains)
          (utils:find-before *rule-separator* body :test #'eq)
        (cl:assert (not (null remains)) nil "Missing rule separator")
        (values (parse-lhs lhs nil)
                (parse-rhs (utils:find-after *rule-separator*
                                             remains :test #'eq)))))))

(defun make-rule-pattern (template location)
  (labels ((build-parsed-pattern (variables form type &optional (binding nil))
             (make-parsed-pattern
              :variables variables
              :class (first form)
              :slots (rest form)
              :type type
              :binding binding
              :address location))
           (parse-pattern (p binding)
             (let ((head (first p)))
               (cl:assert (symbolp head) nil
                 "A symbol doesn't start this pattern: ~S" p)
               (cond ((eq head 'test)
                      (multiple-value-call
                          #'build-parsed-pattern
                        (parse-test-pattern p location) :test))
                     ((eq head 'not)
                      (multiple-value-call
                          #'build-parsed-pattern
                        (parse-default-pattern (second p) location) :negated))
                     ((variablep head)
                      (cl:assert (null binding) nil
                        "Too many pattern variables: ~S" head)
                      (parse-pattern (first (rest p)) head))
                     (t
                      (multiple-value-call
                          #'build-parsed-pattern
                        (parse-default-pattern p location) 
                        :generic binding))))))
    (parse-pattern template nil)))

(defun parse-test-pattern (pattern location)
  (let ((form (rest pattern)))
    (cl:assert (and (listp form)
                    (= (length form) 1)) nil
      "The body of a TEST CE must be a single Lisp form")
    (values
     (loop for obj in (utils:flatten form)
         if (variablep obj)
         collect (cons obj (find-or-set-binding-address obj location)))
     pattern)))

(defun parse-default-pattern (pattern location)
  (let ((head (first pattern)))
    (cl:assert (find-meta-fact head nil) nil
      "This pattern has no meta data: ~S" pattern)
    (labels ((parse-slot (slot)
               (let ((name (first slot))
                     (field (second slot))
                     (constraint (third slot))
                     (bindings nil))
                 (cl:assert (and (symbolp name)
                                 (slot-valuep field)
                                 (constraintp constraint))
                     nil "This pattern has a malformed slot: ~S" pattern)
                 (when (variablep field)
                   (push-slot-binding field name location bindings))
                 (when (consp constraint)
                   (push-test-bindings constraint bindings))
                 (make-pattern-slot :name name :value field
                                    :constraint constraint
                                    :bindings (nreverse bindings))))
             (parse-pattern-body (body slots)
               (let ((slot (first body)))
                 (cl:assert (listp slot) nil
                   "This pattern has structural problems: ~S" body)
                 (if (null slot)
                     (nreverse slots)
                   (parse-pattern-body
                    (rest body)
                    (push (parse-slot slot) slots))))))
      (let ((cleaned-pattern
             (parse-pattern-body (rest pattern) nil)))
        (values (nreverse variables) `(,head ,@cleaned-pattern))))))

;;; End of the rule parsing stuff

(defun normalize-slots (slots)
  (flet ((normalize-slot (slot)
           (let ((slot-name (first slot))
                 (slot-value (second slot)))
             (cl:assert (symbolp slot-name) nil
               "A slot name must be a symbol: ~S" slot-name)
             (if (quotablep slot-value)
                 ``(,',slot-name ,',slot-value)
               ``(,',slot-name ,,slot-value)))))
    `(list ,@(mapcar #'normalize-slot slots))))

(defun canonicalize-slot-names (slots)
  (mapcar #'(lambda (slot)
              `(,(first slot)
                ,(second slot)))
          slots))

(defun parse-and-insert-fact (body)
  (let ((head (first body))
        (slots (rest body)))
    (cl:assert (symbolp head) nil
      "A symbol must start a fact: ~S" body)
    `(assert-fact (current-engine)
                  (make-fact ',head
                             (canonicalize-slot-names 
                              (,@(normalize-slots slots)))))))

(defun parse-and-modify-fact (fact body)
  (flet ((generate-modify ()
           `(modify-fact (current-engine) ,fact
                         (canonicalize-slot-names 
                          (,@(normalize-slots body))))))
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
       (let ((head (first fact)))
         (push (make-fact 
                head (canonicalize-slot-names (rest fact)))
               deffacts)))
     (add-autofact (current-engine)
                   (make-deffacts ',name (nreverse deffacts)))))
       
