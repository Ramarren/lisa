; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-g-symbol.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      10-Oct-90
; Modified:     Thu Apr 29 10:49:59 1993 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-g-symbol.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-g-symbol.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.

(in-package "ZEBU")
;;; Grammar symbols are represented by g-symbol structs.
;;;
;;;
;;; print-name is a string.  
;;;
;;; index is a unique integer associated with the symbol.
;;;
;;; own-productions is a list of the productions that the symbol
;;;                 appears on the left side of.
;;; rhs-productions is a list of the productions the symbol appears
;;;                 on the right side of.
;;;
;;; first-set is the set of terminal grammar symbols which can
;;;                 legally start a string derived from the symbol.
;;;
;;; first-set-dependers is used in the computation of the first-set.
;;;
;;; derives-empty-string is a quick way of telling if the empty
;;;                 string is in the first-set of the symbol.
;;;
;;; follow-set is the set of terminal symbols which may appear after
;;;                 the symbol in strings of the language.
;;;
;;; follow-dependers is the set of grammar symbols whose follow sets
;;;                 must contain this guys follow set.
;;; sets will be represented by o-sets.
;;;
;;; A hack -- a g-symbol is non-terminal if its own-productions is NOT '().

(defstruct (g-symbol (:print-function
		      (lambda (g-symbol stream depth)
			(declare (ignore depth))
			(let ((name (g-symbol-name g-symbol)))
			  (if (g-symbol-non-terminal? g-symbol)
			      (format stream "[<~A>]" name)
			    (format stream "<~A>" name))))))
  name
  index
  (own-productions     '())
  (rhs-productions     '())
  (first-set           (make-oset :order-fn #'g-symbol-order-function))
  (first-set-dependers (make-oset :order-fn #'g-symbol-order-function))
  (derives-empty-string '())
  (follow-set          (make-oset :order-fn #'g-symbol-order-function))
  (follow-dependers    (make-oset :order-fn #'g-symbol-order-function)))


(declaim (inline g-symbol-non-terminal?))
(defun g-symbol-non-terminal? (sym)
  (not (null (g-symbol-own-productions sym))))

(defmacro new-g-symbol (name index)
  `(make-g-symbol :name       ,name
                  :index      ,index))

(declaim (inline g-symbol-order-function))
(defun g-symbol-order-function (sa sb)
  (declare (type g-symbol sa sb))
  (let ((sai (g-symbol-index sa)) (sbi (g-symbol-index sb)))
    (declare (fixnum sai sbi))
    (if (<= sai sbi)
	(if (< sai sbi)
	    'correct-order
	  'equal)
      'wrong-order)))

(declaim (inline g-symbol-add-production))
(defun g-symbol-add-production (g-symbol production)
  (setf (g-symbol-own-productions g-symbol)
	(cons production (g-symbol-own-productions g-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
#||
(load "g-symbol")
(defvar g1 (new-g-symbol "foo" 3))
(defvar g2 (new-g-symbol "goo" 5))
(g-symbol-order-function g1 g2)
(g-symbol-non-terminal? g1)
(print g1)

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              End of zebu-g-symbol.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
