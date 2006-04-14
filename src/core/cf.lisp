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

;;; $Id: cf.lisp,v 1.1 2006/04/14 16:44:37 youngde Exp $

(in-package :lisa.cf)

(defconstant +true+ 1.0)
(defconstant +false+ -1.0)
(defconstant +unknown+ 0.0)

(defun certainty-factor-p (number)
  (<= +false+ number +true+))

(deftype certainty-factor ()
  `(and (real)
        (satisfies certainty-factor-p)))

(defun true-p (cf)
  (check-type cf certainty-factor)
  (> cf +unknown+))

(defun false-p (cf)
  (check-type cf certainty-factor)
  (< cf +unknown+))

(defgeneric certainty-factor (obj)
  (:method ((obj t))
   (error "This object doesn't adhere to the certainty-factor interface: ~S" obj)))

(defun cf-combine (a b)
  (check-type a certainty-factor)
  (check-type b certainty-factor)
  (cond ((and (plusp a)
              (plusp b))
         (+ a b (* -1 a b)))
        ((and (minusp a)
              (minusp b))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defun combine (a b)
  (cf-combine a b))

(defun conjunct-cf (objects)
  "Combines the certainty factors of objects matched within a single rule."
  (let ((conjuncts
         (loop for obj in objects
               for cf = (certainty-factor obj)
               if cf collect cf)))
    (if conjuncts
        (apply #'min conjuncts)
      nil)))

(defun recalculate-cf (objects rule-cf &optional (old-cf nil))
  (let ((conjunct-cf (conjunct-cf objects)))
    (if old-cf
        (the float (combine
                    old-cf (if rule-cf
                           (if conjunct-cf
                               (* rule-cf conjunct-cf)
                             rule-cf)
                         conjunct-cf)))
      (the float (* (if conjunct-cf conjunct-cf rule-cf)
                    (if (and conjunct-cf rule-cf) rule-cf 1.0))))))

(defmethod cf->english (cf)
  (cond ((= cf 1.0) "certain evidence")
        ((> cf 0.8) "strongly suggestive evidence")
        ((> cf 0.5) "suggestive evidence")
        ((> cf 0.0) "weakly suggestive evidence")
        ((= cf 0.0) "no evidence either way")
        ((< cf 0.0) (concatenate 'string (cf->english (- cf)) " against the conclusion"))))