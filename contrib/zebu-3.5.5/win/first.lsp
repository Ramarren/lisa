; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         zebu-first.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Thu Apr 29 10:42:53 1993 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/first.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: first.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
; 27-Mar-92 (Joachim H. Laubsch)
;  modified empty string handling to not propagate to dependers
;  see Fischer LeBlanc, pp 104-106, Grammar G0
; 25-Mar-92 (Joachim H. Laubsch)
;  included warning for non-terminals that do not derive a terminal string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calculate the first sets of the grammar symbols.
;;; Basic design from John Bear :
;;;    University of Texas at Austin Tech Report GRG 220
;;;    "A Breadth-First Syntactic Component"
;;; I added empty string handling: Sandy Wells.

(defun calculate-first-sets ()
  (labels ((calculate-first-sets-aux (prod-lhs prod-rhs)
	     (declare (cons prod-rhs))
	     (let ((rhs-first (car prod-rhs)))
	       (if (g-symbol-non-terminal? rhs-first)
		   ;; must be non terminal
		   ;; X -> Y1 Y2 ... Yn
		   ;; place a in first-sets(X) if for some i a is in first-sets(Yi)
		   ;; and for all j<i empty is in first-sets(Yj)
		   (progn (first-set-add-depender! prod-lhs rhs-first)
			  (if (g-symbol-derives-empty-string rhs-first)
			      (let ((rhs-rest (cdr prod-rhs)))
				(when rhs-rest
				  (calculate-first-sets-aux prod-lhs rhs-rest)))))
		 ;; check for terminal symbol
		 (first-set-insert! rhs-first prod-lhs)))))
    ;; The start set of a terminal symbol is the symbol itself.
    (dolist (gs *symbols*)
      (if (g-symbol-non-terminal? gs)
	  (when (g-symbol-derives-empty-string gs)
	    ;; insert without any propagation to dependers
	    (oset-insert! *empty-string-g-symbol* (g-symbol-first-set gs)))
	(oset-insert! gs (g-symbol-first-set gs))))
    (dolist (prod *productions*)
      (let ((rhs (rhs prod)))
	(if rhs
	    (calculate-first-sets-aux (lhs prod) rhs))))
    (dolist (gs *symbols*)
      (when (g-symbol-non-terminal? gs)
	(let ((x (g-symbol-first-set gs)))
	  (unless (oset-item-list x)
	    (warn "The non-terminal ~A derives no terminal string."
		  (g-symbol-name gs))))))))


;;; Add a symbol to the first set of another symbol.
;;; If it isn't the empty string, and wasn't there already,
;;; add it to the first sets of the guys who's first sets contain this guys.
;;; (the dependers)

(defun first-set-insert! (to-insert insertee)
  (labels ((first-set-insert-aux! (insertee)
	     (when (oset-insert! to-insert (g-symbol-first-set insertee))
	       (dolist (depender (oset-item-list 
				  (g-symbol-first-set-dependers insertee)))
		 (first-set-insert-aux! depender)))))
    (first-set-insert-aux! insertee)))

(defun first-set-add-depender! (new-depender gs)
  (if (oset-insert! new-depender (g-symbol-first-set-dependers gs))
      (dolist (sym (oset-item-list (g-symbol-first-set gs)))
	(unless (eq *empty-string-g-symbol* sym)
	  (first-set-insert! sym new-depender)))))

(defun cruise-first-sets ()
  (dolist (sym *symbols*)
    (format t "~%~A : ~A~%--------------------"
	    (g-symbol-name sym)
	    (with-output-to-string (names)
	      (oset-for-each
	       #'(lambda (ee)
		   (format names "~A  " (g-symbol-name ee)))
	       (g-symbol-first-set sym))))))

;;; first-seq (sequence of symbols) returns {s | seq =*=> s...}

(defun first-seq (seq)
  (declare (type list seq))
  (if (null seq) 
      (make-oset :order-fn #'g-symbol-order-function)
    (let* ((seq1 (car (the cons seq)))
	   (firsts (g-symbol-first-set seq1)))
      (declare (type g-symbol seq1))
      (if (g-symbol-derives-empty-string seq1)
	  (oset-union
	   (oset-delete *empty-string-g-symbol* firsts)
	   (first-seq (cdr seq)))
	firsts))))

;; a specialization to a sequence SEQ, followed by an element SEQ1
(defun first-seq-1 (seq seq1)
  (declare (type list seq) (type g-symbol seq1))
  (labels ((first-seq-aux (seq)
	     (if (null seq)
		 (let ((firsts (g-symbol-first-set seq1)))
		   (if (g-symbol-derives-empty-string seq1)
		       (oset-delete *empty-string-g-symbol* firsts)
		     firsts))
	       (let* ((seq1 (car (the cons seq)))
		      (firsts (g-symbol-first-set seq1)))
		 (declare (type g-symbol seq1))
		 (if (g-symbol-derives-empty-string seq1)
		     (oset-union
		      (oset-delete *empty-string-g-symbol* firsts)
		      (first-seq-aux (cdr seq)))
		   firsts)))))
    (first-seq-aux seq)))
		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
#||
(set-working-directory *ZEBU-test-directory*)
(load-grammar "ex2.zb")
(calculate-empty-string-derivers)
(calculate-first-sets)
(cruise-first-sets)
||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               End of zebu-first.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
