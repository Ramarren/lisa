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
;;; $Id: rule.lisp,v 1.20 2000/12/08 02:04:18 youngde Exp $

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
   (bindings :initform (make-hash-table)
             :accessor get-bindings)
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
  (with-accessors ((actions get-actions)) self
    (format t "Firing rule ~S (token depth ~D)~%"
            (get-name self) (size token))
    (funcall (create-lexical-context self token))))

(defmethod create-lexical-binding ((binding pattern-binding) token)
  (let ((fact (find-fact token (get-location binding))))
    (cl:assert (not (null fact)) ()
      "No fact for location ~D." (get-location binding))
    `(,(get-name binding) ,fact)))

(defmethod create-lexical-binding ((binding slot-binding) token)
  (let ((fact (find-fact token (get-location binding))))
    (cl:assert (not (null fact)) ()
      "No fact for location ~D." (get-location binding))
    `(,(get-name binding) ,(get-slot-value fact (get-slot-name binding)))))
  
(defun create-lexical-bindings (bindings token)
  (let ((vars nil))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (setf vars (nconc vars `(,(create-lexical-binding val token)))))
             bindings)
    (values vars)))
  
#+ignore
(defmethod create-lexical-context ((self rule) token)
  (flet ((make-context ()
           `(lambda ()
              (let (,@(create-lexical-bindings (get-bindings self) token))
                ,@(get-actions self)))))
    (let ((context (make-context)))
      (format t "context: ~S~%" context)
      (eval context))))

(defmethod create-lexical-context ((self rule) token)
  (labels ((create-local-bindings (bindings)
             (with-accessors ((engine get-engine)) self
               (clear-lexical-bindings engine)
               (mapc #'(lambda (binding)
                         (add-lexical-binding engine
                                              (first binding)
                                              (second binding)))
                     bindings)))
           (make-context ()
             (let ((bindings (create-lexical-bindings
                              (get-bindings self) token)))
               (create-local-bindings bindings)
               `(lambda ()
                  (let (,@bindings)
                    ,@(get-actions self))))))
    (let ((context (make-context)))
      (format t "context: ~S~%" context)
      (eval context))))

(defmethod add-binding ((self rule) binding)
  (with-accessors ((bindings get-bindings)) self
    (unless (gethash (get-name binding) bindings)
      (setf (gethash (get-name binding) bindings) binding))))

(defmethod find-binding ((self rule) varname)
  (gethash varname (get-bindings self)))

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

(defmethod record-slot-bindings ((self rule) pattern)
  (declare (type pattern pattern))
  (labels ((create-binding (slot-name test)
             (declare (type test1 test))
             (let ((val (get-value test)))
               (when (variablep val)
                 (add-binding self (make-slot-binding
                                    val (get-location pattern) slot-name)))))
           (create-slot-bindings (slot)
             (declare (type slot slot))
             (mapc #'(lambda (test)
                       (create-binding (get-name slot) test))
                   (get-tests slot))))
    (mapc #'create-slot-bindings (get-slots pattern))))

(defmethod add-pattern ((self rule) pattern)
  (when (has-binding-p pattern)
    (add-binding self (make-pattern-binding
                       (get-pattern-binding pattern)
                       (get-location pattern))))
  (record-slot-bindings self pattern)
  (with-accessors ((patterns get-patterns)) self
    (setf patterns (nconc patterns `(,pattern))))
  (values pattern))

(defmethod freeze-rule ((self rule))
  (when (= (get-pattern-count self) 0)
    (add-pattern self (get-initial-pattern self))))

(defmethod add-node ((self rule) node)
  (with-accessors ((nodes get-nodes)) self
    (setf nodes (nconc nodes `(,node)))
    (increase-use-count node)))

(defmethod get-pattern-count ((self rule))
  (length (get-patterns self)))
  
(defmethod compile-patterns ((self rule) plist)
  (flet ((compile-pattern (p)
           (let ((pattern (parsed-pattern-pattern p))
                 (binding (parsed-pattern-binding p)))
             (add-pattern self
                          (make-pattern (first pattern)
                                        (second pattern) 
                                        (get-pattern-count self)
                                        binding)))))
    (mapc #'compile-pattern plist)))

#+ignore
(defmethod compile-actions ((self rule) rhs)
  (setf (get-actions self) (compile-function rhs)))

(defmethod compile-actions ((self rule) rhs)
  (setf (get-actions self) rhs))

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

