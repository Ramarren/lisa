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

;;; File: rule.lisp
;;; Description: This class represents LISA production rules.
;;;
;;; $Id: rule.lisp,v 1.58 2001/09/13 13:54:09 youngde Exp $

(in-package "LISA")

(defclass rule ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (comment :initform nil
            :initarg :comment
            :accessor get-comment)
   (salience :initform 0
             :initarg :salience
             :accessor get-salience)
   (module :initform nil
           :initarg :module
           :reader get-module)
   (patterns :initform nil
             :accessor get-patterns)
   (actions :initform nil
            :accessor get-actions)
   (bindings :initform (make-binding-table)
             :accessor get-binding-table)
   (nodes :initform nil
          :accessor get-nodes)
   (engine :initarg :engine
           :initform nil
           :reader get-engine)
   (rule-source :initform nil
                :initarg :rule-source
                :reader get-rule-source)
   (initial-pattern :initform (make-generic-pattern 'initial-fact nil nil)
                    :reader get-initial-pattern
                    :allocation :class))
  (:documentation
   "This class represents LISA production rules."))

(defmethod fire ((self rule) token)
  (handler-case
      (evaluate (get-actions self)
                (make-function-context token (get-top-fact token)))
    (error (condition)
      (rule-evaluation-error self condition))))

(defmethod add-binding ((self rule) binding)
  (add-binding (get-binding-table self) binding))

(defun find-binding (self varname)
  (declare (type rule self))
  (lookup-binding (get-binding-table self) varname))

(defmethod get-bindings ((self rule))
  (get-binding-list (get-binding-table self)))

(defun add-new-pattern (rule pattern)
  (with-accessors ((patterns get-patterns)) rule
    (setf patterns (nconc patterns `(,pattern)))))

(defgeneric do-special-ce-handling (rule pattern)
  (:method (rule pattern)
           (declare (ignore rule pattern))
           (values)))

(defmethod do-special-ce-handling ((self rule) (pattern not-pattern))
  (unless (has-patterns-p self)
    (add-new-pattern self (get-initial-pattern self))))

(defgeneric setup-any-pattern-bindings (rule pattern)
  (:method (rule pattern)
           (declare (ignore rule pattern))
           (values)))

(defmethod setup-any-pattern-bindings ((self rule) (pattern bound-pattern))
  (add-binding 
   self (make-pattern-binding 
         (get-pattern-binding pattern) (get-location pattern))))

(defun add-pattern (self pattern)
  (declare (type rule self) (type pattern pattern))
  (finalize-pattern pattern (get-binding-table self))
  (setup-any-pattern-bindings self pattern)
  (do-special-ce-handling self pattern)
  (add-new-pattern self pattern)
  (values pattern))

(defun freeze-rule (self)
  (declare (type rule self))
  (when (zerop (get-pattern-count self))
    (add-new-pattern self (get-initial-pattern self))))

(defun add-new-node (self node)
  (declare (type rule self))
  #+ignore (pushnew node (get-nodes self) :test #'equals)
  (increase-use-count node))

(defmethod add-node ((self rule) node)
  (add-new-node self node))

(defun has-patterns-p (rule)
  (plusp (get-pattern-count rule)))

(defmethod get-pattern-count ((self rule))
  (length (get-patterns self)))
  
(defmethod compile-patterns ((self rule) plist)
  (flet ((compile-pattern (parsed-pattern)
           (add-pattern self
                        (make-pattern parsed-pattern
                                      (get-pattern-count self)))))
    (mapc #'compile-pattern plist)))

(defun add-special-bindings (self bindings)
  (declare (type rule self) (type list bindings))
  (push (make-special-binding +lisa-engine-var+ (get-engine self)) bindings)
  (push (make-special-binding +lisa-rule-var+ self) bindings)
  (values bindings))

#+ignore
(defmethod compile-actions ((self rule) rhs)
  (let ((global-bindings (get-binding-table self))
        (action-bindings nil))
    (flet ((add-binding (var)
             (let ((binding (lookup-binding global-bindings var)))
               (cond ((null binding)
                      (when *show-lisa-warnings*
                        (warn "On the RHS of rule ~S; no binding for variable ~S."
                              (get-name self) var)))
                     (t
                      (setf action-bindings
                        (pushnew binding action-bindings
                                 :key #'get-name)))))))
      (mapc #'(lambda (var) (add-binding var))
            (collect #'(lambda (obj) (variablep obj))
                     (flatten rhs)))
      (setf action-bindings
        (add-special-bindings self action-bindings))
      (setf (get-actions self)
        (make-function-call rhs action-bindings)))))

(defmethod compile-actions ((self rule) rhs)
  (let ((global-bindings (get-binding-table self))
        (action-bindings nil))
    (labels ((add-binding (var)
               (let ((binding (lookup-binding global-bindings var)))
                 (cond ((null binding)
                        (when *show-lisa-warnings*
                          (warn "On the RHS of rule ~S; no binding for variable ~S."
                                (get-name self) var)))
                       (t
                        (setf action-bindings
                          (pushnew binding action-bindings
                                   :key #'get-name)))))))
      (fixup-runtime-bindings rhs #'add-binding)
      (setf action-bindings
        (add-special-bindings self action-bindings))
      (setf (get-actions self)
        (make-function-call rhs action-bindings)))))

(defmethod finalize-rule-definition ((self rule) lhs rhs)
  (compile-patterns self lhs)
  (compile-actions self rhs)
  (values self))

(defmethod print-object ((self rule) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(~S)" (get-name self))))

(defun make-rule (name engine &key (doc-string nil)
                  (salience 0) (module nil) (source nil))
  (make-instance 'rule :name name :engine engine
                 :comment doc-string :salience salience
                 :module module
                 :rule-source source))

