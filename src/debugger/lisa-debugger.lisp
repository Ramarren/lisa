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

;;; $Id: lisa-debugger.lisp,v 1.5 2002/10/28 15:59:20 youngde Exp $

(in-package "LISA")

(defvar *breakpoints* (list))
(defvar *stepping* nil)

(defvar *read-eval-print*)
(defvar *suspended-rule*)
(defvar *tokens*)

(defmacro in-debugger-p ()
  `(cl:assert (boundp '*suspended-rule*) nil
     "The debugger must be running to use this function."))

(defun leave-debugger ()
  (setf *stepping* nil))

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
  (in-debugger-p)
  (setf *stepping* t)
  (setf *read-eval-print* nil)
  (values))

(defun resume ()
  (in-debugger-p)
  (setf *read-eval-print* nil)
  (setf *stepping* nil)
  (values))

(defun tokens (&key (verbose nil))
  (in-debugger-p)
  (format t "Token stack for ~A:~%" (rule-name (rule)))
  (dolist (fact (token-make-fact-list *tokens*))
    (if verbose
        (format t "  ~S~%" fact)
      (format t "  ~A, ~A~%"
              (fact-symbolic-id fact)
              (fact-name fact))))
  (values))

(defun fact (fact-id)
  (find-fact-by-id (inference-engine) fact-id))

(defun bindings ()
  (in-debugger-p)
  (format t "Effective bindings for ~A:~%" (rule-name (rule)))
  (dolist (binding (rule-binding-set (rule)))
    (format t "  ~A: ~S~%"
            (binding-variable binding)
            (if (pattern-binding-p binding)
                (token-find-fact *tokens* (binding-address binding))
              (get-slot-value
               (token-find-fact *tokens* (binding-address binding))
               (binding-slot-name binding)))))
  (values))

(defun debugger-read-eval-print ()
  (flet ((eval-loop ()
           (let ((*terminal-io* *terminal-io*)
                 (*standard-input* *terminal-io*)
                 (*standard-output* *terminal-io*))
             (do ((*read-eval-print* t)
                  (count 0 (incf count)))
                 ((not *read-eval-print*) count)
               (format t "LISA-DEBUG[~D]: " count)
               (force-output)
               (print (eval (read)))
               (terpri)))))
    (handler-case
        (eval-loop)
      (error (e)
        (leave-debugger)
        (error e)))))

(defmethod fire-rule :around ((self rule) tokens)
  (when (or *stepping*
            (has-breakpoint-p self))
    (let ((*active-rule* self)
          (*suspended-rule* self)
          (*tokens* tokens))
      (format t "Stopping in rule ~S~%" (rule-name self))
      (debugger-read-eval-print)))
  (call-next-method))

(defmethod run-engine :after ((self rete) &optional step)
  (leave-debugger))

(provide 'lisa-debugger)
