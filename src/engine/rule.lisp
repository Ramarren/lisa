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
;;; Description: The RULE class.
;;;
;;; $Id: rule.lisp,v 1.6 2000/11/17 02:52:29 youngde Exp $

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
   (nodes :initform nil
          :accessor get-nodes)
   (engine :initarg :engine
           :initform nil
           :reader get-engine)
   (rule-source :initform nil
                :initarg :rule-source
                :reader get-rule-source))
  (:documentation
   "This class represents LISA rules."))

(defmethod fire ((self rule) token)
  (with-accessors ((actions get-actions)) self
    (format t "Firing rule ~S~%" (get-name self))
    (funcall actions)))

(defmethod add-node ((self rule) node)
  (with-accessors ((nodes get-nodes)) self
    (setf nodes (nconc nodes `(,node)))
    (increase-use-count node)))

(defmethod get-pattern-count ((self rule))
  (length (get-patterns self)))
  
(defmethod compile-patterns ((self rule) plist)
  (with-accessors ((patterns get-patterns)) self
    (mapc #'(lambda (p)
              (setf patterns
                (append patterns `(,(make-pattern (first p) (second p))))))
          plist)))

(defmethod compile-actions ((self rule) rhs)
  (setf (get-actions self) (compile-function rhs)))

(defmethod finalize-rule-definition ((self rule) lhs rhs)
  (compile-patterns self lhs)
  (compile-actions self rhs)
  (values self))

(defun make-rule (name engine &key (doc-string nil) (salience 0) (source nil))
  "Constructor for class DEFRULE."
  (make-instance 'rule :name name :engine engine
                 :comment doc-string :salience salience :rule-source source))

