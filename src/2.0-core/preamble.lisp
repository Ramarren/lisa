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

;;; File: preamble.lisp
;;; Description:

;;; $Id: preamble.lisp,v 1.8 2002/11/07 20:03:42 youngde Exp $

(in-package "LISA")

(defvar *active-rule* nil)
(defvar *active-engine* nil)
(defvar *active-tokens* nil)

(defun active-tokens ()
  (declare (inline active-tokens))
  *active-tokens*)

(defun active-rule ()
  (declare (inline active-rule))
  *active-rule*)

(defun active-engine ()
  (declare (inline active-rule))
  *active-engine*)

(defun in-rule-firing-p ()
  (declare (inline in-rule-firing-p))
  (not (null (active-rule))))

(defvar *consider-taxonomy-when-reasoning* nil)

(defmacro consider-taxonomy ()
  `(setf *consider-taxonomy-when-reasoning* t))

(defclass inference-engine-object () ())

(defvar *clear-handlers* (list))

(defmacro register-clear-handler (tag func)
  `(eval-when (:load-toplevel)
     (unless (assoc ,tag *clear-handlers* :test #'string=)
       (setf *clear-handlers*
         (acons ,tag ,func *clear-handlers*)))))

(defun clear-system-environment ()
  (mapc #'(lambda (assoc)
            (funcall (cdr assoc)))
        *clear-handlers*)
  t)

(defun clear-environment-handlers ()
  (setf *clear-handlers* nil))

(defmacro starts-with-? (sym)
  `(eq (aref (symbol-name ,sym) 0) #\?))

(defmacro variablep (sym)
  `(and (symbolp ,sym)
    (starts-with-? ,sym)))

(defmacro quotablep (obj)
  `(and (symbolp ,obj)
        (not (starts-with-? ,obj))))

(defmacro literalp (sym)
  `(or (and (symbolp ,sym)
            (not (variablep ,sym))
            (not (null ,sym)))
       (numberp ,sym) (stringp ,sym)))

(defmacro multifieldp (val)
  `(and (listp ,val)
    (eq (first ,val) 'quote)))

(defmacro slot-valuep (val)
  `(or (literalp ,val)
       (consp ,val)
       (variablep ,val)))

(defmacro constraintp (constraint)
  `(or (null ,constraint)
       (literalp ,constraint)
       (consp ,constraint)))

(defun make-inference-engine ()
  (let ((engine (make-rete)))
    (when (null *active-engine*)
      (setf *active-engine* engine))
    engine))

(defun use-default-engine ()
  "Create and make available a default instance of the inference engine. Use
    this function when you want a basic, single-threaded LISA environment."
  (warn "USE-DEFAULT-ENGINE is deprecated. Use MAKE-INFERENCE-ENGINE instead.")
  (when (null *active-engine*)
    (setf *active-engine* (make-inference-engine)))
  *active-engine*)

(defun current-engine (&optional (errorp t))
  "Returns the currently-active inference engine. Usually only invoked by code
  running within the context of WITH-INFERENCE-ENGINE."
  (when errorp
    (cl:assert (not (null *active-engine*)) (*active-engine*)
      "The current inference engine has not been established."))
  *active-engine*)

(defun inference-engine (&rest args)
  (apply #'current-engine args))

(defmacro with-inference-engine ((engine) &body body)
  "Evaluates BODY within the context of the inference engine ENGINE. This
    macro is MP-safe."
  `(let ((*active-engine* ,engine))
    (progn ,@body)))

(register-clear-handler
 "environment" 
 #'(lambda ()
     (setf *active-engine* (make-inference-engine))))
