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

;;; File:
;;; Description:

;;; $Id: macros.lisp,v 1.8 2001/01/09 01:35:05 youngde Exp $

(in-package :lisa)

(defmacro variablep (sym)
  `(and (symbolp ,sym)
    (eq (elt (symbol-name ,sym) 0) #\?)))

(defmacro literalp (sym)
  `(or (and (symbolp ,sym)
            (not (variablep ,sym)))
       (numberp ,sym) (stringp ,sym)))

(defmacro slot-valuep (val)
  `(or (literalp ,val)
       (consp ,val)
       (variablep ,val)))

(defmacro negated-constraintp (constraint)
  `(and (consp ,constraint)
        (eq (first ,constraint) 'not)
        (not (consp (second ,constraint)))))

(defmacro constraintp (constraint)
  `(or (null ,constraint)
       (literalp ,constraint)
       (consp ,constraint)))

(defmacro assert-conditions ((&rest forms))
  `(unless (and ,@forms)
     (error "ASSERT-CONDITION fired.")))
