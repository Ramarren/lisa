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
;;; Description:

;;; $Id: rule.lisp,v 1.26 2004/03/16 19:40:59 youngde Exp $

(in-package "LISA")

(defclass rule ()
  ((short-name :initarg :short-name
               :initform nil
               :reader rule-short-name)
   (qualified-name :reader rule-name)
   (comment :initform nil
            :initarg :comment
            :reader rule-comment)
   (salience :initform 0
             :initarg :salience
             :reader rule-salience)
   (context :initarg :context
            :reader rule-context)
   (auto-focus :initform nil
               :initarg :auto-focus
               :reader rule-auto-focus)
   (behavior :initform nil
             :accessor rule-behavior)
   (binding-set :initarg :binding-set
                :initform nil
                :reader rule-binding-set)
   (node-list :initform nil
              :reader rule-node-list)
   (activations :initform (make-hash-table :test #'equal)
                :accessor rule-activations)
   (patterns :initform (list)
             :initarg :patterns
             :reader rule-patterns)
   (actions :initform nil
            :initarg :actions
            :reader rule-actions)
   (subrules :initform nil
             :accessor rule-subrules)
   (subrule-p :initform nil
              :accessor subrule-p)
   (logical-marker :initform nil
                   :initarg :logical-marker
                   :reader rule-logical-marker)
   (active-dependencies :initform (make-hash-table :test #'equal)
                        :reader rule-active-dependencies)
   (engine :initarg :engine
           :initform nil
           :reader rule-engine))
  (:documentation
   "Represents production rules after they've been analysed by the language
  parser."))

(defmethod fire-rule ((self rule) tokens)
  (let ((*active-rule* self)
        (*active-engine* (rule-engine self))
        (*active-tokens* tokens))
    (unbind-rule-activation self tokens)
    (funcall (rule-behavior self) tokens)))

(defun rule-default-name (rule)
  (if (initial-context-p (rule-context rule))
      (rule-short-name rule)
    (rule-name rule)))

(defun bind-rule-activation (rule activation tokens)
  (setf (gethash (hash-key tokens) (rule-activations rule))
    activation))

(defun unbind-rule-activation (rule tokens)
  (remhash (hash-key tokens) (rule-activations rule)))

(defun clear-activation-bindings (rule)
  (clrhash (rule-activations rule)))

(defun find-activation-binding (rule tokens)
  (gethash (hash-key tokens) (rule-activations rule)))

(defun attach-rule-nodes (rule nodes)
  (setf (slot-value rule 'node-list) nodes))

(defun compile-rule-behavior (rule actions)
  (setf (rule-behavior rule)
    (make-behavior (rule-actions-actions actions)
                   (rule-actions-bindings actions))))

(defmethod conflict-set ((self rule))
  (conflict-set (rule-context self)))

(defmethod print-object ((self rule) strm)
  (print-unreadable-object (self strm :type t)
    (format strm "~A"
            (if (initial-context-p (rule-context self))
                (rule-short-name self)
              (rule-name self)))))

(defun compile-rule (rule patterns actions)
  (compile-rule-behavior rule actions)
  (add-rule-to-network (rule-engine rule) rule patterns)
  rule)

(defun composite-rule-p (rule)
  (consp (rule-subrules rule)))

(defun add-subrule (rule subrule)
  (setf (subrule-p subrule) t)
  (push subrule (rule-subrules rule)))

(defun logical-rule-p (rule)
  (numberp (rule-logical-marker rule)))

(defun auto-focus-p (rule)
  (rule-auto-focus rule))

(defun find-any-logical-boundaries (patterns)
  (flet ((ensure-logical-blocks-are-valid (addresses)
           (cl:assert (and (= (first (last addresses)) 1)
                           (eq (parsed-pattern-class (first patterns))
                               'initial-fact)) nil
             "Logical patterns must appear first within a rule.")
           ;; BUG FIX - FEB 17, 2004 - Aneil Mallavarapu
           ;;         - replaced: 
           ;; (reduce #'(lambda (first second) 
           ;; arguments need to be inverted because address values are PUSHed
           ;; onto the list ADDRESSES, and therefore are in reverse order
           (reduce #'(lambda (second first)
                       (cl:assert (= second (1+ first)) nil
                         "All logical patterns within a rule must be contiguous.")
                       second)
                   addresses :from-end t)))
    (let ((addresses (list)))
      (dolist (pattern patterns)
        (when (logical-pattern-p pattern)
          (push (parsed-pattern-address pattern) addresses)))
      (unless (null addresses)
        (ensure-logical-blocks-are-valid addresses))
      (first addresses))))

(defmethod initialize-instance :after ((self rule) &rest initargs)
  (setf (slot-value self 'qualified-name)
    (intern (format nil "~A.~A" 
                    (context-name (rule-context self))
                    (rule-short-name self))))
  self)
                    
(defun make-rule (name engine patterns actions 
                  &key (doc-string nil) 
                       (salience 0) 
                       (context (active-context))
                       (auto-focus nil))
  (flet ((make-rule-binding-set ()
           (delete-duplicates
            (loop for pattern in patterns
                append (parsed-pattern-binding-set pattern)))))
    (compile-rule
     (make-instance 'rule 
       :short-name name 
       :engine engine
       :patterns patterns
       :actions actions
       :comment doc-string
       :salience salience
       :context (if (null context)
                    (find-context (inference-engine) :initial-context)
                  (find-context (inference-engine) context))
       :auto-focus auto-focus
       :logical-marker (find-any-logical-boundaries patterns)
       :binding-set (make-rule-binding-set))
     patterns actions)))

(defun make-composite-rule (name engine patterns actions
                            &rest args
                            &key &allow-other-keys)
  (flet ((make-composite-name (index)
           (intern (format nil "~A~~~D" name index))))
    (let ((primary-rule
           (apply #'make-rule name engine (first patterns) actions args))
          (index 0))
      (dolist (pattern (rest patterns) primary-rule)
        (add-subrule 
         primary-rule
         (apply #'make-rule
                (make-composite-name (incf index))
                engine pattern actions args))))))
    
(defun copy-rule (rule engine)
  (let ((initargs `(:doc-string ,(rule-comment rule)
                    :salience ,(rule-salience rule)
                    :context ,(rule-context rule)
                    :auto-focus ,(rule-auto-focus rule))))
    (if (composite-rule-p rule)
        (apply #'make-composite-rule
               (rule-short-name rule)
               engine
               (append `(,(rule-patterns rule))
                       `(,@(mapcar #'rule-patterns (rule-subrules rule))))
               (rule-actions rule)
               initargs)
      (apply #'make-rule
             (rule-short-name rule)
             engine
             (rule-patterns rule)
             (rule-actions rule)
             initargs))))
