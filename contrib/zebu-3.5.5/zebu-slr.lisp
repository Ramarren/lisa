; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-slr.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:       1-Nov-90
; Modified:     Fri Mar  8 14:46:41 1996 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-slr.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-slr.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.

;;;
;;; Do all needed to build an slr table starting with a lisp syntax grammar.
(in-package "ZEBU")

(defun slr-tables-from-grammar (file-name &rest args)
  (apply #'load-grammar file-name args)
  (calculate-empty-string-derivers)
  (calculate-first-sets)
  (calculate-follow-sets)
  (make-lr0-collection)
  (build-parse-tables nil)
  file-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test:

#||
(slr-tables-from-grammar "ex1.zb")
(format t "symbols: ~%")
(cruise-symbols-2)
(format t "productions: ~%")
(print-productions)
(format t "lr0 item sets: ~%")
(print-collection nil)
(format t "slr tables: ~%")
(cruise-parse-tables)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                End of zebu-slr.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
