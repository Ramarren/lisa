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

;;; File: language.lisp
;;; Description: Code that implements the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.3 2002/08/28 20:18:34 youngde Exp $

(in-package "LISA")

(defmacro defrule (name (&key (salience 0) (module nil)) &body body)
  (let ((rule-name (gensym)))
    `(let ((,rule-name ,@(if (consp name)
                             `(,name)
                             `(',name))))
       (redefine-defrule ,rule-name
                         ',body
                         :salience ,salience
                         :module ,module))))

(defmacro deftemplate (name (&key) &body body)
  (redefine-deftemplate name body))

(defmacro defimport (class-name &key (use-inheritancep t))
  `(import-class ,class-name ,use-inheritancep))

(defun assert-instance (instance)
  (parse-and-insert-instance instance))

(defmacro assert ((&body body))
  (parse-and-insert-fact body))
