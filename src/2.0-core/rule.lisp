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

;;; $Id: rule.lisp,v 1.17 2002/11/14 18:21:46 youngde Exp $

(in-package "LISA")

(defclass rule ()
  ((name :initarg :name
         :initform nil
         :reader rule-name)
   (comment :initform nil
            :initarg :comment
            :reader rule-comment)
   (salience :initform 0
             :initarg :salience
             :reader rule-salience)
   (module :initform nil
           :initarg :module
           :reader rule-module)
   (behavior :initform nil
             :accessor rule-behavior)
   (binding-set :initarg :binding-set
                :initform nil
                :reader rule-binding-set)
   (node-list :initform nil
              :reader rule-node-list)
   (activations :initform (make-hash-table :test #'equal)
                :accessor rule-activations)
   (subrules :initform nil
             :accessor rule-subrules)
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

(defmethod print-object ((self rule) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(~S)" (rule-name self))))

(defun compile-rule (rule patterns actions)
  (compile-rule-behavior rule actions)
  (add-rule-to-network (rule-engine rule) rule patterns)
  rule)

(defun composite-rule-p (rule)
  (consp (rule-subrules rule)))

(defun add-subrule (rule subrule)
  (push subrule (rule-subrules rule)))

(defun logical-rule-p (rule)
  (numberp (rule-logical-marker rule)))

(defun find-any-logical-boundaries (patterns)
  (flet ((ensure-logical-blocks-are-valid (addresses)
           (cl:assert (and (= (first (last addresses)) 1)
                           (eq (parsed-pattern-class (first patterns))
                               'initial-fact)) nil
             "Logical patterns must appear first within a rule.")
           (reduce #'(lambda (first second)
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

(defun make-rule (name engine patterns actions 
                  &key (doc-string nil) (salience 0) (module nil))
  (flet ((make-rule-binding-set ()
           (delete-duplicates
            (loop for pattern in patterns
                append (parsed-pattern-binding-set pattern)))))
    (compile-rule
     (make-instance 'rule 
       :name name 
       :engine engine
       :comment doc-string
       :salience salience
       :module module
       :logical-marker (find-any-logical-boundaries patterns)
       :binding-set (make-rule-binding-set))
     patterns actions)))

(defun make-composite-rule (name engine patterns actions
                            &rest args
                            &key &allow-other-keys)
  (flet ((make-composite-name (index)
           (intern (format nil "~S..~D" name index))))
    (let ((primary-rule
           (apply #'make-rule name engine (first patterns) actions args))
          (index 0))
      (dolist (pattern (rest patterns) primary-rule)
        (add-subrule 
         primary-rule
         (apply #'make-rule
                (make-composite-name (incf index))
                engine pattern actions args))))))
    
