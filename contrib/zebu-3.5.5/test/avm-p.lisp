; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         av-printers.l
; Description:  
; Author:       Joachim H. Laubsch
; Created:      13-Apr-92
; Modified:     Thu Oct  2 12:49:53 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/test/Attic/avm-p.lisp,v 1.1 2000/10/12 02:39:45 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: avm-p.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:45  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "CL-USER")

(defun print-FEAT-TERM (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~@[type: ~S ~][~{~S~^ ~}]"
	  (FEAT-TERM--type ITEM)
	  (FEAT-TERM--slots ITEM)))

(defun print-General-Var (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "%~S"
	  (General-Var--name ITEM)))

(defun print-Label-value-pair (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "(~S ~S)"
	  (Label-value-pair--label ITEM)
	  (Label-value-pair--value ITEM)))

(defun PRINT-TAGGED-TERM (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~S=~S"
	  (Tagged-Term--tag ITEM)
	  (Tagged-Term--term ITEM)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            End of av-printers.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
