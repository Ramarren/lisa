; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         zebu-follow.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Tue Jan 26 09:21:04 1993 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/follow.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: follow.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
; 20-Mar-91 (Joachim H. Laubsch)
;  Improved grammar debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute follow on a grammar symbol.

(defun calculate-follow-sets ()
  (compute-follow-dependers)
  (follow-insert-first-sets))


;;; Called initially on a production with prod being the production and
;;; prod-rhs being the rhs of the production.
;;; Returns true only if the prod-rhs derives the empty string, or is the
;;; empty string.  Fills in follow set dependencies by side effect.

(defun compute-follow-dependers (&aux prod)
  (labels ((compute-follow-dependers-aux (prod-rhs)
	     (if prod-rhs
		 (let ((rhs-first (car prod-rhs)))
		   (when (compute-follow-dependers-aux (cdr prod-rhs))
		     (oset-insert! rhs-first
				   (g-symbol-follow-dependers (lhs prod)))
		     ;; Return indication of whether tail derives empty string.
		     (g-symbol-derives-empty-string rhs-first)))
	       t)))
    (do ((prods *productions* (cdr prods)))
	((null prods))
      (setq prod (car (the cons prods)))
      (compute-follow-dependers-aux (rhs prod)))))

(defun follow-insert-first-sets ()
  (labels ((follow-insert-symbol (symbol-to-insert whose-follow-set)
	     ;; Both arguments are g-symbols.
	     (if (oset-insert! symbol-to-insert
			       (g-symbol-follow-set whose-follow-set))
		 ;; Do it to his dependers too..
		 (dolist (depender (oset-item-list (g-symbol-follow-dependers
						    whose-follow-set)))
		   (follow-insert-symbol symbol-to-insert depender))))
	   (follow-insert-first-sets-aux (prod-rest)
	     ;; Called on successive tails of the rhs of each production.
	     (when prod-rest
	       (let ((prod-rest2 (cdr prod-rest)))
		 (when prod-rest2
		   ;; prod-rest has at least two items
		   (dolist (symbol (oset-item-list (first-seq prod-rest2)))
		     (unless (eq symbol *empty-string-g-symbol*)
		       (follow-insert-symbol symbol (car prod-rest))))
		   (follow-insert-first-sets-aux prod-rest2))))))
    (follow-insert-symbol *the-end-g-symbol* *start-symbol*)
    (dolist (prod *productions*)
      (follow-insert-first-sets-aux (rhs prod)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test:

#||
(set-working-directory *ZEBU-test-directory*)
(load-grammar "ex2.zb")
(compile-slr-grammar "ex2.zb")
(ZEBU-LOAD-FILE "ex2.tab")
(calculate-empty-string-derivers)
(calculate-first-sets)
(calculate-follow-sets)
(cruise-follow-sets)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               End of zebu-follow.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
