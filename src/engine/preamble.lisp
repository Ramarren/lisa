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

;;; $Id: preamble.lisp,v 1.10 2002/05/26 16:02:07 youngde Exp $

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

(defgeneric mark-instance-as-changed (instance &key (engine nil) (slot-id nil))
  (:method ((instance t) &key engine slot-id)
           (declare (ignore slot-id) (ignore engine))
           (error
            "LISA does not have a MARK-INSTANCE-AS-CHANGED method defined for class ~S."
            (class-of instance)))
  (:documentation
   "This generic function describes the protocol by which LISA can be notified
   that a CLOS instance has been changed outside of LISA's control. Primary
   methods on this function are automatically generated as classes are
   registered with LISA."))

(defgeneric tell-lisa-modified-instance (instance slot)
  (:method ((instance t) slot)
           (declare (ignore slot))
           t)
  (:documentation
   "This generic function serves as a notification protocol, whereby LISA
   informs an application whenever a rule firing modifies an instance
   slot. Interested code should provide its own specialized method(s)."))

