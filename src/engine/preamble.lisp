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
;;; Description: Stuff here must be built before the engine module.

;;; $Id: preamble.lisp,v 1.12 2002/08/06 01:17:03 youngde Exp $

(in-package "LISA")

(defconstant +lisa-engine-var+ 
    (intern (symbol-name (gensym "?")))
  "Used to represent the variable bound to a rule's RETE instance and
  accessible from a rule's RHS.")

(defconstant +lisa-rule-var+
    (intern (symbol-name (gensym "?")))
  "Used to represent the variable bound to a rule's CLOS instance and
    accessible from a rule's RHS.")

(defvar *during-rule-execution* nil
  "This variable is bound to T whenever LISA is executing a rule.")

(defvar *show-lisa-warnings* t
  "Bind this variable to NIL if you want to inhibit LISA warning messages.")

(defgeneric equals (object-1 object-2))

(defmethod equals ((a symbol) (b symbol))
  (eq a b))
