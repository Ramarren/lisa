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

;;; File: macros.lisp
;;; Description: General LISA macros.

;;; $Id: macros.lisp,v 1.18 2001/03/15 16:00:30 youngde Exp $

(in-package "LISA")

(defmacro starts-with-? (sym)
  `(eq (elt (symbol-name ,sym) 0) #\?))

(defmacro variablep (sym)
  `(and (symbolp ,sym)
    (starts-with-? ,sym)))

(defmacro quotablep (obj)
  `(and (symbolp ,obj)
        (not (starts-with-? ,obj))))

(defmacro literalp (sym)
  `(or (and (symbolp ,sym)
            (not (variablep ,sym))
            (not (null ,sym)))
       (numberp ,sym) (stringp ,sym)))

(defmacro slot-valuep (val)
  `(or (literalp ,val)
       (consp ,val)
       (variablep ,val)))

(defmacro constraintp (constraint)
  `(or (null ,constraint)
       (literalp ,constraint)
       (consp ,constraint)))

(defmacro assert-conditions ((&rest forms))
  `(unless (and ,@forms)
     (error "ASSERT-CONDITION fired.")))
