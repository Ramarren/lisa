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

;;; $Id: conditions.lisp,v 1.7 2001/03/31 20:56:30 youngde Exp $

(in-package "LISA")

(define-condition lisa-error (error)
  ((text :initarg :text
         :initform nil))
  (:documentation
   "The base class of the LISA condition hierarchy."))

(define-condition syntactical-error (lisa-error)
  ((element :initarg :element
            :initform nil))
  (:documentation
   "This condition represents syntactical errors discovered during the initial
   parsing pass."))

(define-condition environment-error (lisa-error)
  ()
  (:documentation
   "This condition represents LISA environmental errors."))

(define-condition rule-structure-error (lisa-error)
  ((rule-name :initarg :rule-name)
   (element :initarg :element
            :initform nil))
  (:report
   (lambda (condition strm)
     (with-slots (rule-name element text) condition
       (format strm "While compiling rule ~S~%" rule-name)
       (unless (null element)
         (format strm "While parsing element ~S~%" element))
       (format strm text)))
   :documentation
   "This condition represents structural errors found while parsing DEFRULE
   forms."))

(define-condition command-structure-error (lisa-error)
  ((command-name :initarg :command-name))
  (:report
   (lambda (condition strm)
     (with-slots (command-name text) condition
       (format strm "While evaluating the LISA function ~S~%" command-name)
       (format strm text)))
   :documentation
   "This condition represents structural errors found while parsing specific
   LISA functions."))

(defmacro pattern-error (pattern format-string &rest args)
  `(error 'syntactical-error
    :element ,pattern
    :text (apply #'format nil ,format-string `(,,@args))))

(defmacro parsing-error (format-string &rest args)
  `(error 'syntactical-error
    :text (apply #'format nil ,format-string `(,,@args))))

(defmacro rule-structure-error (rule-name parse-condition)
  `(with-slots (text element) ,parse-condition
    (error 'rule-structure-error
     :rule-name ,rule-name :element element :text text)))

(defmacro command-structure-error (command-name parse-condition)
  `(with-slots (text) ,parse-condition
    (error 'command-structure-error
     :command-name ,command-name :text text)))

(defmacro environment-error (format-string &rest args)
  `(error 'environment-error
    :text (apply #'format nil ,format-string `(,,@args))))
