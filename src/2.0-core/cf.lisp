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

;;; $Id: cf.lisp,v 1.11 2004/09/17 19:08:24 youngde Exp $

(in-package :lisa.cf)

(defconstant +true+ 1.0)

(defconstant +false+ -1.0)

(defgeneric cf-combine (a b)
  (:method ((a t) (b t))
   (error "Either one or both arguments is not a legal certainty factor: ~A, ~A." a b)))

(defmethod cf-combine ((a number) (b number))
  (cond ((and (plusp a)
              (plusp b))
         (+ a b (* -1 a b)))
        ((and (minusp a)
              (minusp b))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defgeneric cf-p (cf)
  (:method ((cf number))
   (<= +false+ cf +true+))
  (:method ((cf t))
   nil))

(defmethod combine ((a number) (b number) &rest args)
  (declare (ignore args))
  (cf-combine a b))

(defmethod conjunct-cf (facts)
  (let ((conjuncts
         (let ((list (list)))
           (map 'list #'(lambda (fact)
                          (let ((cf (lisa:cf fact)))
                            (when (plusp cf)
                              (push cf list))))
                facts)
           list)))
    (cond (conjuncts
           (if (= (length conjuncts) 1)
               (first conjuncts)
             (apply #'min conjuncts)))
          (t nil))))

(defmethod recalculate-cf (fact-list rule-cf &optional (cf nil))
  (let ((conjunct-cf (conjunct-cf fact-list)))
    (if cf
        (the float (cf:combine
                    cf (if rule-cf
                           (if conjunct-cf
                               (* rule-cf conjunct-cf)
                             rule-cf)
                         conjunct-cf)))
      (the float (* (if conjunct-cf conjunct-cf rule-cf)
                    (if (and conjunct-cf rule-cf) rule-cf 1.0))))))

