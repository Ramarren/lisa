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

;;; File: context.lisp
;;; Description:

;;; $Id: context.lisp,v 1.5 2002/11/19 19:53:19 youngde Exp $

(in-package "LISA")

(defclass context ()
  ((name :initarg :name
         :reader context-name)
   (rules :initform (make-hash-table)
          :reader context-rules)
   (strategy :initarg :strategy
             :reader context-strategy)))

(defmethod print-object ((self context) strm)
  (print-unreadable-object (self strm :type t)
    (if (initial-context-p self)
        (format strm "~S" "The Initial Context")
      (format strm "~A" (context-name self)))))

(defun find-rule-in-context (context rule-name)
  (values (gethash rule-name (context-rules context))))

(defun add-rule-to-context (context rule)
  (setf (gethash (rule-name rule) (context-rules context)) rule))

(defmethod remove-rule-from-context ((self context) (rule-name symbol))
  (remhash rule-name (context-rules self)))

(defmethod remove-rule-from-context ((self context) (rule t))
  (remhash (rule-name rule) (context-rules self)))

(defun clear-activations (context)
  (remove-activations (context-strategy context)))

(defun context-activation-list (context)
  (list-activations (context-strategy context)))

(defun context-rule-list (context)
  (loop for rule being the hash-value of (context-rules context)
      collect rule))

(defun clear-context (context)
  (clear-activations context)
  (clrhash (context-rules context)))

(defun initial-context-p (context)
  (string= (context-name context) "INITIAL-CONTEXT"))

(defun make-context-name (defined-name)
  (typecase defined-name
    (symbol (symbol-name defined-name))
    (string defined-name)
    (otherwise
     (error "The context name must be a string designator."))))

(defmacro with-context (context &body body)
  `(let ((*active-context* ,context))
     ,@body))

(defun make-context (name &key (strategy nil))
  (make-instance 'context
    :name (make-context-name name)
    :strategy (if (null strategy)
                  (make-breadth-first-strategy)
                strategy)))
