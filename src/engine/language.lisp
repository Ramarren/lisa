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
;;; $Id: language.lisp,v 1.17 2000/11/17 21:45:22 youngde Exp $

(in-package :lisa)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defrule assert defimport halt)))

(defmacro defrule (name &body body)
  "Creates or redefines a rule in the network."
  `(redefine-defrule ',name ',body))

(defmacro assert ((&body body))
  `(parse-and-insert-fact ',body))

(defmacro defimport (name class)
  `(import-and-register-class ',name ',class))

(defmacro reset ()
  `(reset-engine (current-engine)))

(defmacro run ()
  `(run-engine (current-engine)))

(defmacro halt ()
  `(values))
