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

;;; File:
;;; Description:

;;; $Id: contexts.lisp,v 1.1 2002/11/21 15:04:04 youngde Exp $

(in-package "LISA-USER")

(defcontext :hobbits)
(defcontext :wizards)
(defcontext :elves)

(deftemplate frodo ()
  (slot name))

(deftemplate gandalf ()
  (slot name))

(deftemplate legolas ()
  (slot name))

(defrule frodo (:context :hobbits)
  (frodo)
  =>
  (format t "frodo firing; focusing on :wizards.~%")
  (assert (gandalf (name gandalf)))
  (focus :wizards))

(defrule gandalf (:context :wizards)
  (gandalf (name gandalf))
  =>
  (format t "gandalf firing; refocusing on :hobbits.~%")
  (refocus))

(defrule legolas (:context :elves)
  (legolas)
  =>
  (format t "legolas firing; hopefully this was a manual focus.~%"))

(defrule start ()
  =>
  (focus :hobbits))
