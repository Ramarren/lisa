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

;;; $Id: macros.lisp,v 1.14 2001/01/28 20:29:32 youngde Exp $

(in-package :lisa)

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
            (not (variablep ,sym)))
       (numberp ,sym) (stringp ,sym)))

(defmacro slot-valuep (val)
  `(or (literalp ,val)
       (consp ,val)
       (variablep ,val)))

(defmacro negated-rewritable-constraintp (constraint)
  `(and (consp ,constraint)
        (eq (first ,constraint) 'not)
        (not (consp (second ,constraint)))))

(defmacro negated-rewritable-literal-constraintp (constraint)
  `(and (consp ,constraint)
        (eq (first ,constraint) 'not)
        (literalp (second ,constraint))))

(defmacro constraintp (constraint)
  `(or (null ,constraint)
       (literalp ,constraint)
       (consp ,constraint)))

(defmacro oreq (var &rest args)
  `(or ,@(mapcar #'(lambda (obj)
                     `(equal ,var ,(if (symbolp obj) `,obj obj)))
                 args)))

(defmacro neq (obj-1 obj-2)
  `(not (equal ,obj-1 ,obj-2)))

(defmacro assert-conditions ((&rest forms))
  `(unless (and ,@forms)
     (error "ASSERT-CONDITION fired.")))
