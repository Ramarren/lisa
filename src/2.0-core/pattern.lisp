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

;;; File: pattern.lisp
;;; Description: Structures here collectively represent patterns after they've
;;; been analysed by the language parser. This is the canonical representation
;;; of parsed patterns that Rete compilers are intended to see.

;;; $Id: pattern.lisp,v 1.10 2002/09/03 01:48:26 youngde Exp $

(in-package "LISA")

(defstruct pattern-slot
  "Represents the canonical form of a slot within a pattern analysed by the
  language parser. NAME is the slot identifier, as a symbol; VALUE is the
  slot's value, and its type can be one of (symbol number string list) or a
  LISA variable; CONSTRAINT, if not NIL, represents a constraint placed on the
  slot's value. CONSTRAINT should only be non-NIL if VALUE is a variable, and
  can be one of the types listed for VALUE or a CONS representing arbitrary
  Lisp code; BINDINGS is a list of tuples representing variable bindings
  associated with the slot. Each tuple is of the form (variable slot-name
  address); VARIABLE is the symbol in the slot's VALUE field, SLOT-NAME is the
  bound to VARIABLE, and ADDRESS is the location of the pattern holding the
  VARIABLE declaration." 
  (name nil :type symbol)
  (value nil)
  (slot-binding nil :type list)
  (constraint nil)
  (constraint-bindings nil :type list))

(defstruct parsed-pattern
  "Represents the canonical form of a pattern analysed by the language
  parser. CLASS is the name, or head, of the pattern, as a symbol; SLOTS is a
  list of PATTERN-SLOT objects representing the analysed slots of the pattern;
  ADDRESS is a small integer representing the pattern's position within the
  rule form, starting at 0; BINDING, if not NIL, is the variable to which a
  token matching the pattern will be bound during the match process; VARIABLES
  is a list of all variables found in the pattern's slots; TYPE is one of
  (:GENERIC :NEGATED :TEST) and indicates the kind of pattern represented."
  (class nil :type symbol)
  (slots nil :type list)
  (address nil :type integer)
  (pattern-binding nil :type symbol)
  (type nil :type symbol))

(defstruct rule-actions
  (bindings nil :type list)
  (actions nil :type list))

(defun simple-slot-p (pattern-slot)
  (not (variablep (pattern-slot-value))))

(defun bound-pattern-p (parsed-pattern)
  (not (null (parsed-pattern-pattern-binding parsed-pattern))))
