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

;;; File: pattern-compiler.lisp
;;; Description:

;;; $Id: pattern-compiler.lisp,v 1.3 2002/08/29 15:29:25 youngde Exp $

(in-package "LISA")

(defstruct compiled-pattern
  (intra-pattern-tests nil :type list)
  (inter-pattern-tests nil :type list))

(defun make-class-test (class-name)
  (function
   (lambda (token)
     (eq class-name
         (fact-name
          (peek-fact-in-token token))))))

(defun make-simple-slot-test (slot-name value)
  (function
   (lambda (token)
     (equal value
            (get-slot-value
             (peek-fact-in-token token)
             slot-name)))))

(defun compile-pattern (parsed-pattern))
