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

;;; File: activation.lisp
;;; Description: This class represents an activation of a rule.

;;; $Id: activation.lisp,v 1.2 2002/09/19 23:04:35 youngde Exp $

(in-package "LISA")

(defclass activation ()
  ((rule :initarg :rule
         :initform nil
         :reader activation-rule)
   (tokens :initarg :tokens
           :initform nil
           :reader activation-tokens)
   (eligible :initform t
             :accessor activation-eligible))
  (:documentation
   "Represents a rule activation."))

(defmethod fire-activation ((self activation))
  (fire-rule (activation-rule self) (activation-tokens self)))

(defun eligible-p (activation)
  (activation-eligible activation))

(defun inactive-p (activation)
  (not (eligible-p activation)))

(defmethod print-object ((self activation) strm)
  (let ((tokens (activation-tokens self))
        (rule (activation-rule self)))
    (print-unreadable-object (self strm :identity t :type t)
      (format strm "(~S ~S ; salience = ~D)"
              (rule-name rule)
              (mapcar #'get-symbolic-id (get-all-facts token))
              (rule-salience rule)))))

(defmethod hash-code ((self activations))
  (activation-tokens self))

(defun make-activation (rule tokens)
  (make-instance 'activation :rule rule :tokens tokens))

