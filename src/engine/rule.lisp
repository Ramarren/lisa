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
;;; File: rule.lisp
;;; Description: This class represents LISA production rules.
;;;
;;; $Id: rule.lisp,v 1.38 2001/01/26 20:42:02 youngde Exp $

(in-package :lisa)

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

#+ignore
(defmethod fire ((self rule) token)
  (with-accessors ((actions get-actions)) self
    (evaluate (make-function-call
               (get-actions self) (get-bindings self))
              (make-function-context token (get-top-fact token)))))

(defmethod fire ((self rule) token)
  (evaluate (get-actions self)
            (make-function-context token (get-top-fact token))))

(defmethod add-binding ((self rule) binding)
  (add-binding (get-binding-table self) binding))

(defmethod find-binding ((self rule) varname)
  (lookup-binding (get-binding-table self) varname))

(defmethod get-bindings ((self rule))
  (get-binding-list (get-binding-table self)))

(defmethod traverse-bindings ((self rule) token)
  (flet ((show-binding (b)
           (let ((fact (find-fact token (get-location b))))
             (format t "~S, ~S~%" b fact))))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (show-binding val))
             (get-bindings self))))

(defmethod traverse-token ((self rule) token)
  (labels ((traverse (token)
             (cond ((null token)
                    (values))
                   (t
                    (format t "~S~%" token)
                    (traverse (get-parent token))))))
    (traverse token)))

(defun add-new-pattern (rule pattern)
  (with-accessors ((patterns get-patterns)) rule
    (setf patterns (nconc patterns `(,pattern)))))

(defmethod do-special-ce-handling ((self rule) (pattern not-pattern))
  (unless (has-patterns-p self)
    (add-new-pattern self (get-initial-pattern self))))

(defmethod do-special-ce-handling ((self rule) (pattern generic-pattern))
  (values))

(defmethod add-pattern ((self rule) pattern)
  (finalize-pattern pattern (get-binding-table self))
  (when (has-binding-p pattern)
    (add-binding self (make-pattern-binding
                       (get-pattern-binding pattern)
                       (get-location pattern))))
  (do-special-ce-handling self pattern)
  (add-new-pattern self pattern)
  (values pattern))

(defmethod freeze-rule ((self rule))
  (when (zerop (get-pattern-count self))
    (add-pattern self (get-initial-pattern self))))

(defun add-new-node (rule node)
  (declare (type rule rule))
  (with-accessors ((nodes get-nodes)) rule
    (setf nodes (nconc nodes `(,node)))
    (increase-use-count node)))

(defmethod add-node ((self rule) (node node-test))
  (unless (member node (get-nodes self) :test #'equals)
    (add-new-node self node)))

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

#+ignore
(defmethod compile-actions ((self rule) rhs)
  (setf (get-actions self) rhs))

(defmethod compile-actions ((self rule) rhs)
  (setf (get-actions self)
    (make-function-call rhs (get-bindings self))))

(defmethod finalize-rule-definition ((self rule) lhs rhs)
  (compile-patterns self lhs)
  (compile-actions self rhs)
  (values self))

(defmethod print-object ((self rule) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(~S)" (get-name self))))

(defun make-rule (name engine &key (doc-string nil) (salience 0) (source nil))
  "Constructor for class DEFRULE."
  (make-instance 'rule :name name :engine engine
                 :comment doc-string :salience salience :rule-source source))

