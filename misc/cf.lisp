;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

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

;;; File: cf.lisp
;;; Description: Test code for Lisa's certainty factor support.

;;; $Id: cf.lisp,v 1.3 2004/09/15 18:22:38 youngde Exp $

(in-package :lisa-user)

(defclass hobbit ()
  ((name :initarg :name
         :reader name)))

(defclass has-ring () ())

(defclass two-clowns () ())

(defrule frodo ()
  (hobbit (name frodo))
  =>
  (assert (has-ring) :cf 0.9))

(defrule bilbo ()
  (hobbit (name bilbo))
  =>
  (assert (has-ring) :cf 0.3))

(defrule combine ()
  (?a (hobbit (name merry)))
  (?b (hobbit (name pippin)))
  =>
  (assert (two-clowns) :cf (cf:combine ?a ?b)))

(defrule combine-2 (:cf 0.9)
  (?a (hobbit (name sam)))
  (?b (hobbit (name bilbo)))
  =>
  (assert (two-clowns) :cf (cf:combine ?a ?b)))

  