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

;;; File: pattern.lisp
;;; Description: Base class for all types of patterns found on a rule LHS.

;;; $Id: pattern.lisp,v 1.55 2001/09/06 00:04:24 youngde Exp $

(in-package "LISA")

(defclass pattern ()
  ((name :initarg :name
         :reader get-name)
   (bindings :initform (list)
             :reader get-bindings)
   (location :initarg :location
             :reader get-location))
  (:documentation
   "Base class for all types of patterns found on a rule LHS."))

(defstruct (parsed-pattern
            (:constructor make-internal-parsed-pattern))
  (pattern nil :type list)
  (binding nil :type symbol)
  (type nil :type symbol))

;;; This function traverses a parsed but uncompiled pattern and "fixes up" the
;;; runtime bindings of any special variables. In other words, given a pattern
;;; of the form (ROCKY (NAME ?NAME) (BUDDY ?BUDDY)) FIXUP-RUNTIME-BINDINGS
;;; will look at each variable and replace it with its SYMBOL-VALUE if that
;;; variable is BOUNDP. This binding typically occurs whenever a rule is
;;; dynamically defined; i.e. it is created at runtime through the execution
;;; of another rule.

(defun fixup-runtime-bindings (pattern)
  (labels ((fixup-bindings (part result)
             (declare (optimize (speed 3) (debug 1) (safety 1)))
             (let ((token (first part))
                   (new-token nil))
               (cond ((null token)
                      (return-from fixup-bindings (nreverse result)))
                     ((and (variablep token)
                           (boundp token))
                      (setf new-token (symbol-value token)))
                     ((listp token)
                      (setf new-token (fixup-bindings token nil)))
                     (t
                      (setf new-token token)))
               (fixup-bindings (rest part) (push new-token result)))))
    (fixup-bindings pattern nil)))

(defun make-parsed-pattern (&key pattern binding type)
  (make-internal-parsed-pattern
   :pattern (if *during-rule-execution*
                (fixup-runtime-bindings pattern)
              pattern)
   :binding binding
   :type type))
