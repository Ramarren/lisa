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

;;; File: debugger.lisp
;;; Description: The LISA debugger.

;;; $Id: lisa-debugger.lisp,v 1.2 2002/10/24 14:28:17 youngde Exp $

(in-package "LISA")

(defvar *breakpoints* (list))
(defvar *read-eval-print* nil)
(defvar *tokens* nil)
(defvar *stepping* nil)

(defun has-breakpoint-p (rule)
  (find (rule-name rule) *breakpoints*))

(defun breakpoints ()
  (format t "Current breakpoints:~%")
  (dolist (rule-name *breakpoints*)
    (format t "  ~A~%" rule-name))
  (values))

(defun set-break (rule-name)
  (if (find-rule (inference-engine) rule-name)
      (pushnew rule-name *breakpoints*)
    (format t "There's no rule by this name (~A)~%" rule-name))
  (values))

(defun clear-break (rule-name)
  (setf *breakpoints*
    (delete rule-name *breakpoints*))
  (values))

(defun clear-breaks ()
  (setf *breakpoints* (list))
  (values))

(defun next ()
  (setf *stepping* t)
  (setf *read-eval-print* nil))

(defun resume ()
  (setf *read-eval-print* nil)
  (setf *stepping* nil)
  (values))

(defun debugger-read-eval-print ()
  (let ((*terminal-io* *terminal-io*)
        (*standard-input* *terminal-io*)
        (*standard-output* *terminal-io*))
    (do ((*read-eval-print* t)
         (count 0 (incf count)))
        ((not *read-eval-print*) (values))
      (format t "LISA-DEBUG[~D]: " count)
      (force-output)
      (print (eval (read)))
      (terpri))))

(defmethod fire-rule :around ((self rule) tokens)
  (when (or *stepping*
            (has-breakpoint-p self))
    (let ((*active-rule* self)
          (*tokens* tokens))
      (format t "Stopping in rule ~S~%" (rule-name self))
      (debugger-read-eval-print)))
  (call-next-method))

(provide 'lisa-debugger)
