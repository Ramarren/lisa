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

;;; File: mycin.lisp
;;; Description: An implementation of MYCIN as illustrated in PAIP, pg. 553. This example
;;; is used to exercise Lisa's new support for certainty factors.

;;; $Id: mycin.lisp,v 1.2 2004/09/15 19:23:57 youngde Exp $

(in-package :lisa-user)

(defclass param-mixin ()
  ((value :initarg :value
          :initform nil
          :reader value)
   (entity :initarg :entity
           :initform nil
           :reader entity)))

(defclass culture () ())

(defclass culture-site (param-mixin) ())

(defclass culture-age (param-mixin) ())

(defclass patient ()
  ((name :initarg :name
         :initform nil
         :reader name)
   (sex :initarg :sex
        :initform nil
        :reader sex)
   (age :initarg :age
        :initform nil
        :reader age)))

(defclass burn (param-mixin) ())

(defclass compromised-host (param-mixin) ())

(defclass organism () ())

(defclass gram (param-mixin) ())

(defclass morphology (param-mixin) ())

(defclass aerobicity (param-mixin) ())

(defclass growth-conformation (param-mixin) ())

(defclass organism-identity (param-mixin) ())

(defrule rule-52 (:cf 0.4)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod) (entity ?organism))
  (burn (value serious))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule rule-71 (:cf 0.7)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus) (entity ?organism))
  (growth-conformation (value clumps) (entity ?organism))
  =>
  (assert (organism-identity (value staphylococcus) (entity ?organism))))

(defrule rule-73 (:cf 0.9)
  (culture-site (value blood))
  (gram (value neg) (entity ?organism))
  (morphology (value rod) (entity ?organism))
  (aerobicity (value anaerobic) (entity ?organism))
  =>
  (assert (organism-identity (value bacteroides) (entity ?organism))))

(defrule rule-75 (:cf 0.6)
  (gram (value neg) (entity ?organism))
  (morphology (value rod) (entity ?organism))
  (compromised-host (value t))
  =>
  (assert (organism-identity (value pseudomonas) (entity ?organism))))

(defrule rule-107 (:cf 0.8)
  (gram (value neg) (organism ?organism))
  (morphology (value rod) (entity ?organism))
  (aerobicity (value aerobic) (entity ?organism))
  =>
  (assert (organism-identity (value enterobacteriaceae) (entity ?organism))))

(defrule rule-165 (:cf 0.7)
  (gram (value pos) (entity ?organism))
  (morphology (value coccus) (entity ?organism))
  (growth-conformation (value chains) (entity ?organism))
  =>
  (assert (organism-identity (value streptococcus) (entity ?organism))))

(defun culture-1 ()
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Sylvia Fischer"
                                 :sex 'female
                                 :age 27)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (burn (value serious) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (culture-age (value 3)))
    (assert (gram (value neg) (entity ?organism)))
    (assert (morphology (value rod) (entity ?organism)))
    (assert (aerobicity (value aerobic) (entity ?organism)))
    (run)))

(defun culture-2 ()
  (reset)
  (let ((?organism (make-instance 'organism))
        (?patient (make-instance 'patient
                                 :name "Sylvia Fischer"
                                 :sex 'female
                                 :age 27)))
    (assert (compromised-host (value t) (entity ?patient)))
    (assert (burn (value serious) (entity ?patient)))
    (assert (culture-site (value blood)))
    (assert (culture-age (value 3)))
    (assert (gram (value neg) (entity ?organism)) :cf 0.8)
    (assert (gram (value pos) (entity ?organism)) :cf 0.2)
    (assert (morphology (value rod) (entity ?organism)))
    (assert (aerobicity (value anaerobic) (entity ?organism)))
    (run)))
