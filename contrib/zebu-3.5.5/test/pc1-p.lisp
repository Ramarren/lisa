; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         pc1-p.lisp
; Description:  
; Author:       Joachim H. Laubsch
; Created:      13-Apr-92
; Modified:     Thu Oct  2 12:57:16 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/test/Attic/pc1-p.lisp,v 1.1 2000/10/12 02:39:45 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: pc1-p.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:45  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "CL-USER")

(defun PRINT-PROPOSITIONAL-VARIABLE (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~S"
	  (PROPOSITIONAL-VARIABLE--name ITEM)))

(defun PRINT-BOOLEAN-OR (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~S or ~S"
	  (BOOLEAN-EXPR--RAND1 ITEM)
	  (BOOLEAN-EXPR--RAND2 ITEM)))

(defun PRINT-BOOLEAN-AND (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~S and ~S"
	  (BOOLEAN-EXPR--RAND1 ITEM)
	  (BOOLEAN-EXPR--RAND2 ITEM)))

(defun print-atomic-wff (item stream level)
  (declare (ignore level))
  (format stream
	  "~a(~a)"
	  (atomic-wff--predicate item)
	  (atomic-wff--role-argument-pairs item)))

(defun print-role-argument-pair (item stream level)
  (declare (ignore level))
  (format stream
	  "~a: ~a"
	  (role-argument-pair--role item)
	  (role-argument-pair--argument item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         end of pc1-p.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
