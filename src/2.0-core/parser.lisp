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
;;; $Id: parser.lisp,v 1.52 2002/11/04 19:25:48 youngde Exp $

(in-package "LISA")

(defconstant *rule-separator* '=>)

(defvar *compound-patterns-p*)
(defvar *binding-table*)

(defun make-binding-set ()
  (loop for binding being the hash-value of *binding-table*
      collect binding))

(defun find-or-set-slot-binding (var slot-name location)
  (multiple-value-bind (binding existsp)
      (gethash var *binding-table*)
    (unless existsp
      (setf binding
        (setf (gethash var *binding-table*)
          (make-binding var location slot-name))))
    (values binding existsp)))

(defun find-slot-binding (var &key (errorp t))
  (let ((binding (gethash var *binding-table*)))
    (when errorp
      (cl:assert (not (null binding)) nil
        "There's no slot binding for variable ~S" var))
    binding))

(defun set-pattern-binding (var location)
  (cl:assert (null (gethash var *binding-table*)) nil
    "This is a duplicate pattern binding: ~S" var)
  (setf (gethash var *binding-table*)
    (make-binding var location :pattern)))

(defun collect-bindings (forms &key (errorp t))
  (let ((bindings (list)))
    (dolist (obj (utils:flatten forms))
      (when (variablep obj)
        (let ((binding (find-slot-binding obj :errorp errorp)))
          (unless (null binding)
            (push binding bindings)))))
    (nreverse bindings)))

(defmacro with-rule-components (((doc-string lhs rhs) rule-form) &body body)
  (let ((remains (gensym)))
    `(let ((*binding-table* (make-hash-table))
           (*compound-patterns-p* nil))
       (multiple-value-bind (,doc-string ,remains)
           (extract-rule-headers ,rule-form)
         (multiple-value-bind (,lhs ,rhs)
             (parse-rulebody ,remains)
           ,@body)))))

(defun make-composite-ruleset (patterns)
  (let ((rulesets (list)))
    (labels ((build-ruleset (patterns ruleset)
               (let ((pattern (first patterns)))
                 (cond ((endp patterns) 
                        (push ruleset rulesets))
                       ((compound-pattern-p pattern)
                        (dolist (sub-pattern 
                                    (parsed-pattern-sub-patterns pattern))
                          (build-ruleset
                           (rest patterns)
                           (append ruleset `(,sub-pattern))))
                        ruleset)
                       (t 
                        (build-ruleset
                         (rest patterns)
                         (append ruleset `(,pattern))))))))
      (build-ruleset patterns nil)
      (nreverse rulesets))))

(defun define-rule (name body &optional (salience 0) (module nil))
  (with-rule-components ((doc-string lhs rhs) body)
    #+ignore (format t "LHS: ~S~%" lhs)
    #+ignore (format t "RHS: ~S~%" rhs)
    (if *compound-patterns-p*
        (make-composite-rule 
         name (inference-engine) 
         (make-composite-ruleset lhs) rhs
         :doc-string doc-string
         :salience salience
         :module module)
      (make-rule name (inference-engine) lhs rhs
                 :doc-string doc-string
                 :salience salience
                 :module module))))

(defun redefine-defrule (name body &key (salience 0) (module nil))
  (define-rule name body salience module))

(defun extract-rule-headers (body)
  (if (stringp (first body))
      (values (first body) (rest body))
    (values nil body)))

(defun fixup-runtime-bindings (patterns)
  (labels ((fixup-bindings (part result)
             (let* ((token (first part))
                    (new-token token))
               (cond ((null part)
                      (return-from fixup-bindings (nreverse result)))
                     ((and (variablep token)
                           (boundp token))
                      (setf new-token (symbol-value token)))
                     ((consp token)
                      (setf new-token (fixup-bindings token nil))))
               (fixup-bindings (rest part) (push new-token result)))))
    (fixup-bindings patterns nil)))

(defun preprocess-left-side (lhs)
  (cond ((null lhs)
         (push (list 'initial-fact) lhs))
        ((rule)
         (fixup-runtime-bindings lhs))
        (t lhs)))

(defvar *in-logical-pattern-p* nil)

(defun parse-rulebody (body)
  (let ((location -1)
        (patterns (list)))
    (labels ((parse-lhs (body)
               (let ((pattern (first body)))
                 (cl:assert (listp pattern) nil
                   "This pattern is malformed: ~S" pattern)
                 (cond ((endp body)
                        (unless *in-logical-pattern-p*
                          (reverse patterns)))
                       ((eq (first pattern) 'logical)
                        (let ((*in-logical-pattern-p* t))
                          (parse-lhs (rest pattern)))
                        (parse-lhs (rest body)))
                       (t
                        (push
                         (make-rule-pattern 
                          pattern (incf location)) patterns)
                        (parse-lhs (rest body))))))
             (parse-rhs (actions) 
               (make-rule-actions
                :bindings (collect-bindings actions :errorp nil)
                :actions actions)))
      (multiple-value-bind (lhs remains)
          (utils:find-before *rule-separator* body :test #'eq)
        (cl:assert (not (endp remains)) nil "Missing rule separator")
        (values (parse-lhs (preprocess-left-side lhs))
                (parse-rhs (utils:find-after *rule-separator*
                                             remains :test #'eq)))))))

(defun make-rule-pattern (template location)
  (labels ((build-parsed-pattern (form bindings type 
                                  &optional (binding nil))
             (make-parsed-pattern
              :test-bindings bindings
              :class (first form)
              :slots (rest form)
              :type type
              :pattern-binding binding
              :binding-set (make-binding-set)
              :logical *in-logical-pattern-p*
              :address location))
           (build-compound-pattern (sub-patterns)
             (make-parsed-pattern
              :type :or
              :sub-patterns sub-patterns
              :logical *in-logical-pattern-p*
              :address location))
           (parse-pattern (p &optional binding)
             (let ((head (first p)))
               (cl:assert (symbolp head) nil
                 "A symbol doesn't start this pattern: ~S" p)
               (cond ((eq head 'test)
                      (multiple-value-call
                          #'build-parsed-pattern
                        (parse-test-pattern p) :test))
                     ((eq head 'not)
                      (multiple-value-call
                          #'build-parsed-pattern
                        (parse-default-pattern (second p) location) :negated))
                     ((variablep head)
                      (parse-pattern (second p)
                                     (set-pattern-binding head location)))
                     ((eq head 'or)
                      (setf *compound-patterns-p* t)
                      (build-compound-pattern
                       (mapcar #'parse-pattern (rest p))))
                     (t
                      (multiple-value-call
                          #'build-parsed-pattern
                        (parse-default-pattern p location) 
                        :generic binding))))))
    (parse-pattern template nil)))

(defun parse-test-pattern (pattern)
  (let ((form (rest pattern)))
    (cl:assert (and (listp form)
                    (= (length form) 1)) nil
      "The body of a TEST CE must be a single Lisp form")
    (values pattern (collect-bindings form))))

(defun parse-default-pattern (pattern location)
  (let ((head (first pattern)))
    (cl:assert (find-meta-fact head nil) nil
      "This pattern has no meta data: ~S" pattern)
    (macrolet ((collect-constraint-bindings (constraint list)
                 `(progn
                    (dolist (obj (utils:flatten ,constraint))
                      (when (variablep obj)
                        (pushnew (find-slot-binding obj)
                                 ,list :key #'first)))
                    ,list))
               (simple-form-p (form)
                 `(atom ,form))
               (simple-negated-form-p (form)
                 `(and (consp ,form)
                       (eq (first ,form) 'not)
                       (atom (second ,form))))
               (check-for-intra-pattern-bindings (bindings)
                 `(and (consp ,bindings)
                       (every #'(lambda (b)
                                  (= location (binding-address b)))
                              ,bindings)))
               (make-equality-predicate (var value)
                 `(progn
                    `(equal ,,var ,@(if (symbolp ,value) 
                                        `(',,value) `(,,value))))))
            (labels ((parse-constraint (var constraint)
                 (let ((bindings (list)))
                   (cond ((simple-form-p constraint)
                          (values (make-equality-predicate var constraint)
                                  (list (find-slot-binding var)) nil))
                         ((simple-negated-form-p constraint)
                          (values (make-equality-predicate var (second constraint))
                                  (list (find-slot-binding var)) t))
                         (t
                          (values constraint
                                  (collect-constraint-bindings
                                   constraint bindings)
                                  nil)))))
               (parse-slot (slot)
                 (let ((name (first slot))
                       (field (second slot))
                       (constraint (third slot))
                       (slot-binding nil)
                       (negated nil)
                       (existing-bindings (list))
                       (constraint-bindings (list)))
                   (cl:assert (and (symbolp name)
                                   (slot-valuep field)
                                   (constraintp constraint))
                       nil "This pattern has a malformed slot: ~S" pattern)
                   (when (simple-negated-form-p field)
                     (setf field (second field))
                     (setf negated t))
                   (when (variablep field)
                     (multiple-value-bind (binding exists-p)
                         (find-or-set-slot-binding field name location)
                       (setf slot-binding binding)
                       (when (or exists-p
                                 (not (null constraint)))
                         (push binding existing-bindings))))
                   (unless (null constraint)
                     (multiple-value-setq (constraint constraint-bindings negated)
                       (parse-constraint field constraint))
                     (setf existing-bindings
                       (append existing-bindings constraint-bindings)))
                   (make-pattern-slot :name name 
                                      :value field
                                      :slot-binding slot-binding
                                      :negated negated
                                      :intra-pattern-bindings
                                      (check-for-intra-pattern-bindings
                                       existing-bindings)
                                      :constraint constraint
                                      :constraint-bindings constraint-bindings)))
               (parse-pattern-body (body slots)
                 (let ((slot (first body)))
                   (cl:assert (listp slot) nil
                     "This pattern has structural problems: ~S" body)
                   (if (null slot)
                       (nreverse slots)
                     (parse-pattern-body
                      (rest body)
                      (push (parse-slot slot) slots))))))
        (values
         `(,head ,@(parse-pattern-body (rest pattern) nil)) nil)))))

;;; End of the rule parsing stuff

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

(defun bind-logical-dependencies (fact)
  (dolist (index (rule-logicals (active-rule)) fact)
    (add-dependent-fact
     (token-find-fact (active-tokens) index) fact)))
  
(defun parse-and-insert-instance (instance)
  (let ((fact
         (make-fact-from-instance 
          (find-symbolic-name instance) instance)))
    (when (and (in-rule-firing-p)
               (logical-rule-p (active-rule)))
      (bind-logical-dependencies fact))
  (assert-fact (current-engine) fact)))

(defun parse-and-retract-instance (instance)
  (let ((engine (current-engine)))
    (retract-fact engine (find-fact-using-instance engine instance))))

(defun show-deffacts (deffact)
  (format t "~S~%" deffact)
  (values deffact))

(defun parse-and-insert-deffacts (name body)
  (let ((deffacts (gensym)))
    `(let ((,deffacts (list)))
       (dolist (fact ',body)
         (let ((head (first fact)))
           (push 
            (apply #'make-fact head (rest fact))
            ,deffacts)))
       (add-autofact (inference-engine)
                     (make-deffacts ',name (nreverse ,deffacts))))))
       
