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

;;; $Id: rule.lisp,v 1.7 2002/10/09 18:24:45 youngde Exp $

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
   (node-list :initform nil
              :reader rule-node-list)
   (engine :initarg :engine
           :initform nil
           :reader rule-engine))
  (:documentation
   "Represents production rules after they've been analysed by the language
  parser."))

(defvar *merge* nil)

(defmethod initialize-instance :after ((self rule) &key patterns actions)
  (add-rule-to-network (rule-engine self) self patterns)
  (compile-rule-behavior self actions)
  self)

#+ignore
(defmethod initialize-instance :after ((self rule) &key patterns actions)
  (if *merge*
      (merge-rule-into-network (rete-network (rule-engine self))
                               patterns self)
    (compile-rule-into-network 
     (rete-network (rule-engine self)) patterns self))
  (compile-rule-behavior self actions)
  self)

#+ignore
(defmethod initialize-instance :after ((self rule) &key patterns actions)
  (compile-rule-into-network 
   (rete-network (rule-engine self)) patterns self)
  (compile-rule-behavior self actions)
  self)

(defmethod fire-rule ((self rule) tokens)
  (let ((*active-rule* self)
        (*active-engine* (rule-engine self)))
    (funcall (rule-behavior self) tokens)))

(defun attach-rule-nodes (rule nodes)
  (setf (slot-value rule 'node-list) nodes))

(defun compile-rule-behavior (rule actions)
  (setf (rule-behavior rule)
    (make-behavior (rule-actions-actions actions)
                   (rule-actions-bindings actions))))

(defmethod print-object ((self rule) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(~S)" (rule-name self))))

(defun make-rule (name engine patterns actions 
                  &key (doc-string nil) (salience 0) (module nil))
  (make-instance 'rule 
    :name name 
    :engine engine
    :comment doc-string
    :salience salience
    :module module
    :patterns patterns
    :actions actions))

