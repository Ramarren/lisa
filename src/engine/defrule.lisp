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
;;; File: defrule.lisp
;;; Description: The DEFRULE class.
;;;
;;; $Id: defrule.lisp,v 1.6 2000/10/25 15:18:29 youngde Exp $

(in-package "LISA")

(defvar *rete* (make-hash-table))

(defclass defrule ()
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
   (rule-source :initform nil
                :initarg :rule-source
                :reader get-rule-source))
  (:documentation
   "This class represents LISA rules."))

(defmethod compile-patterns ((rule defrule) plist)
  (with-accessors ((patterns get-patterns)) rule
    (mapc #'(lambda (p)
              (format t "compiling pattern: ~S~%" p)
              (setf patterns
                (append patterns `(,(make-pattern (first p) (second p))))))
          plist)))

(defmethod compile-actions ((rule defrule) rhs)
  (setf (get-actions rule) (compile-function rhs)))

(defmethod finalize-rule-definition ((rule defrule) lhs rhs)
  (compile-patterns rule lhs)
  (compile-actions rule rhs)
  (setf (gethash (get-name rule) *rete*) rule)
  (values rule))

(defun make-defrule (name &key (doc-string nil) (salience 0) (source nil))
  "Constructor for class DEFRULE."
  (make-instance 'defrule :name name :comment doc-string
                 :salience salience :rule-source source))

