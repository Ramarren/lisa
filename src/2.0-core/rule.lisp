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

;;; $Id: rule.lisp,v 1.1 2002/08/23 15:47:07 youngde Exp $

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
   (binding-table :initform (make-hash-table)
                  :accessor get-binding-table)
   (engine :initarg :engine
           :initform nil
           :reader get-engine))
  (:documentation
   "Represents production rules after they've been analysed by the language
  parser."))

(defun compile-rule-patterns (rule patterns)
  (setf (get-patterns rule) patterns))

(defun compile-rule-actions (rule actions))

(defun finalize-rule-definition (rule patterns actions)
  (compile-rule-patterns rule patterns)
  (compile-rule-actions rule actions)
  rule)

(defmethod print-object ((self rule) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(~S)" (get-name self))))

(defun make-rule (name engine &key (doc-string nil)
                  (salience 0) (module nil))
  (make-instance 'rule :name name :engine engine
                 :comment doc-string :salience salience
                 :module module))


