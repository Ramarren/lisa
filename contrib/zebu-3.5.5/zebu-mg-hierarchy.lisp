; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-mg-hierarchy.lisp
; Description:  types and printers for the meta grammar
; Author:       Joachim H. Laubsch
; Created:      13-May-92
; Modified:     Thu Dec 21 11:50:12 1995 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-mg-hierarchy.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-mg-hierarchy.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
(require "zebu-aux")
(provide "zebu-mg-hierarchy")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Top of hierarchy for ZEBU META-Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFSTRUCT (ZEBU-MG (:INCLUDE KB-DOMAIN)
                    (:CONSTRUCTOR NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 KB-SEQUENCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *kb-sequence-separator* " "
  "A string, separating the elements of a KB-sequence")

(defstruct (KB-SEQUENCE (:include ZEBU-MG)
			(:print-function KB-SEQUENCE-print)) 
  first
  (rest nil :type (or NULL KB-SEQUENCE)))

(defun KB-SEQUENCE-print (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (if (KB-SEQUENCE-p ITEM)
      (let ((first (KB-SEQUENCE-first ITEM))
	    (rest  (KB-SEQUENCE-rest ITEM)))
	(if (null rest)
	    (format STREAM "~a" first)
	  (if (kb-sequence-p rest)
	      (format STREAM "~a~:{~A~a~}"
		      first
		      (labels ((cons-kb-seq (seq)
				 (if (null seq)
				     nil
				   (cons (list *kb-sequence-separator*
					       (KB-SEQUENCE-first seq))
					 (cons-kb-seq
					  (KB-SEQUENCE-rest seq))))))
			(cons-kb-seq rest)))
	    (format STREAM "~a~A~a" first *kb-sequence-separator* rest))))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  FEAT-TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFSTRUCT (FEAT-TERM (:INCLUDE Zebu-mg)
		      #||
		      (:print-function
		       (lambda (ITEM STREAM LEVEL)
			 (DECLARE (IGNORE LEVEL))
			 (format STREAM
				 "~@[type: ~S ~][~{~S~^ ~}]"
				 (FEAT-TERM--type ITEM)
				 (FEAT-TERM--slots ITEM))))
		      ||#
		      )
  -TYPE
  (-SLOTS nil))

(DEFSTRUCT (LABEL-VALUE-PAIR (:INCLUDE ZEBU-MG)
			     #||
			     (:print-function
			      (lambda (ITEM STREAM LEVEL)
				(DECLARE (IGNORE LEVEL))
				(format STREAM
					"(~S ~S)"
					(Label-value-pair--label ITEM)
					(Label-value-pair--value ITEM))))
			     ||#
			     )
           -LABEL
           (-VALUE nil))

#|| Not used yet
(DEFSTRUCT (GENERAL-VAR (:INCLUDE ZEBU-MG)
			#||
			(:print-function
			 (lambda (ITEM STREAM LEVEL)
			   (DECLARE (IGNORE LEVEL))
			   (format STREAM
				   "%~S"
				   (General-Var--name ITEM))))
			||#
			)
           -NAME)

(DEFSTRUCT (TAGGED-TERM (:INCLUDE ZEBU-MG)
			#||
			(:print-function
			 (lambda (ITEM STREAM LEVEL)
			   (DECLARE (IGNORE LEVEL))
			   (format STREAM
				   "~S=~S"
				   (Tagged-Term--tag ITEM)
				   (Tagged-Term--term ITEM))))
			||#
			)
           -TERM
           -TAG)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               PRODUCTION-RHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFSTRUCT (PRODUCTION-RHS (:INCLUDE ZEBU-MG)
			   #||
			   (:print-function print-production-rhs)
			   ||#
			   )
  (-SYNTAX nil)
  (-SEMANTICS nil)
  -BUILD-FN)

(defun print-production-rhs (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~{~S ~}~@[ { ~S }~];"
	  (production-rhs--syntax ITEM)
	  (production-rhs--semantics ITEM)))

(DEFSTRUCT (Kleene (:INCLUDE ZEBU-MG) )
           -constituent
           -separator)

(DEFSTRUCT (Kleene* (:INCLUDE Kleene) ))
(DEFSTRUCT (Kleene+ (:INCLUDE Kleene) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (domain-type (:include zebu-mg))
  -supertype -type -slots print-function)

(defun cons-domain-type (name avm print-function)
  ;; Return: [supertype type slots print-function]
  (let ((type (if (feat-term-p avm)
		  (feat-term--type avm)
		'KB-Domain))
	(slots (if (feat-term-p avm)
		   (feat-term--slots avm)
		 avm)))
    (make-domain-type
     :-supertype type
     :-type name
     :-slots (mapcar #'(lambda (slot)
			 (let ((v (label-value-pair--value slot)))
			   (if (null v)
			       (label-value-pair--label slot)
			     (list (label-value-pair--label slot) v))))
		     slots)
     :print-function print-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      End of zebu-mg-hierarchy.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
