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
;;; File: language.lisp
;;; Description: Code that implements the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.29 2001/03/13 18:54:58 youngde Exp $

(in-package :lisa)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defrule assert defimport reset clear run halt facts rules retract
            modify)))

(defmacro defrule (name &body body)
  "Creates or redefines a rule in the network."
  `(redefine-defrule ',name ',body))

(defmacro deftemplate (name &body body)
  `(redefine-deftemplate ',name ',body))

(defmacro assert ((&body body))
  (parse-and-insert-fact body))

(defmacro defimport (name class)
  `(import-and-register-class ',name ',class))

(defmacro facts ()
  `(print-fact-list (current-engine)))

(defmacro rules ()
  `(print-rule-list (current-engine)))

(defmacro agenda ()
  `(print-activation-list (current-engine)))

(defmacro reset ()
  `(reset-engine (current-engine)))

(defmacro clear ()
  `(clear-engine (current-engine)))

(defmacro run (&optional (step t))
  `(run-engine (current-engine) ,step))

(defmacro retract (fact-id)
  `(retract-fact (current-engine) ,fact-id))

(defmacro modify (fact &body body)
  (parse-and-modify-fact fact body))

(defmacro watch (event)
  `(watch-event ,event))

(defmacro unwatch (event)
  `(unwatch-event ,event))

(defmacro watching ()
  `(format t "Watching: ~S~%" (get-watches)))

(defmacro halt ()
  `(values))

(defun print-activation-list (engine)
  (let ((activations (get-activation-list engine)))
    (mapc #'(lambda (act)
              (format t "~S~%" act))
          activations)
    (format t "For a total of ~D activation~:P.~%"
            (length activations))
    (values)))

(defun print-rule-list (engine)
  (let ((rules (get-rule-list engine)))
    (mapc #'(lambda (rule)
              (format t "~S~%" rule))
          rules)
    (format t "For a total of ~D rule~:P.~%" (length rules))
    (values)))

(defun print-fact-list (engine)
  (let ((facts (get-fact-list engine)))
    (mapc #'(lambda (fact)
              (format t "~S~%" fact))
          facts)
    (format t "For a total of ~D fact~:P.~%" (length facts))
    (values)))
