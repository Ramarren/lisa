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

;;; File: conditions.lisp
;;; Description: This file contains the condition hierarchy and error recovery
;;; support for LISA.

;;; $Id: conditions.lisp,v 1.4 2001/03/30 20:25:44 youngde Exp $

(in-package "LISA")

(define-condition lisa-condition (error)
  ()
  (:documentation
   "The base class of the LISA condition hierarchy."))

(define-condition syntactical-error (lisa-condition)
  ((text :initarg :text))
  (:documentation
   "This condition represents syntactical errors discovered during the initial
   parsing pass."))

(define-condition rule-structure-error (lisa-condition)
  ((rule-name :initarg :rule-name)
   (text :initarg :text))
  (:report
   (lambda (condition strm)
     (with-slots (rule-name text) condition
       (format strm "While parsing rule ~S:~%" rule-name)
       (format strm text)))
   :documentation
   "This condition represents structural errors found while parsing DEFRULE
   forms."))

(define-condition command-structure-error (lisa-condition)
  ((command-name :initarg :command-name)
   (text :initarg :text))
  (:report
   (lambda (condition strm)
     (with-slots (command-name text) condition
       (format strm "While evaluating the LISA function ~S:~%" command-name)
       (format strm text)))
   :documentation
   "This condition represents structural errors found while parsing specific
   LISA functions."))

(defmacro parsing-error (format-string &rest args)
  `(error 'syntactical-error
    :text (apply #'format nil ,format-string `(,,@args))))

(defmacro rule-structure-error (rule-name parse-condition)
  `(with-slots (text) ,parse-condition
    (error 'rule-structure-error
     :rule-name ,rule-name :text text)))

(defmacro command-structure-error (command-name parse-condition)
  `(with-slots (text) ,parse-condition
    (error 'command-structure-error
     :command-name ,command-name :text text)))
