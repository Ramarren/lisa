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
;;; Description: Supporting code for Lisa's uncertainty mechanism.

;;; $Id: cf.lisp,v 1.2 2004/09/13 19:27:47 youngde Exp $

(in-package :lisa.cf)

(defconstant +true+ 1.0)

(defconstant +false+ -1.0)

(defgeneric cf-or (a b)
  (:method ((a t) (b t))
   (error "Either one or both arguments is not a legal certainty factor: ~A, ~A." a b)))

(defgeneric cf-and (a b)
  (:method ((a t) (b t))
   (error "Either one or both arguments is not a legal certainty factor: ~A, ~A." a b)))

(defmethod cf-or ((a number) (b number))
  (cond ((and (plusp a)
              (plusp b))
         (+ a b (* -1 a b)))
        ((and (minusp a)
              (minusp b))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defmethod cf-and ((a number) (b number))
  (min a b))

(defmethod cf-p ((cf number))
  (<= +false+ cf +true+))

(defmethod combine (a b)
  (cf-or a b))