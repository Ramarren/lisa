; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         zebu-lalr1.l
; Description:  Calculation of LALR(1) sets
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Fri Mar  8 14:48:03 1996 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/lalr1.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: lalr1.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Propagate lookaheads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is used when we discover that lookaheads propagate from one
;;; lr(0) item set to another during the calculation of lalr(1) sets
;;; of items.  Add a link to the dependency digraph and propagate the
;;; lookaheads we already know about.

(declaim (inline lalr1-add-depender lalr1-add-lookahead))

;;; This is used when we discover a lookhead for an lr(0) item set during
;;; the calculation of lalr(1) sets.  If the lookahead wasn't already there,
;;; add it, and also add it to the "dependers": those item sets to whom
;;; lookaheads propagate from the item in question.

(defun lalr1-add-lookahead (symbol item)
  (declare (type item item))
  (labels ((lalr1-add-lookahead-aux (item)
	     (when (oset-insert! symbol (item-look-aheads item))
	       ;; Wasn't already there.
	       (dolist (depender
			 (the list (oset-item-list
				    (the oset
					 (item-look-ahead-dependers item)))))
		 (lalr1-add-lookahead-aux depender)))))
    (lalr1-add-lookahead-aux item)))

(defun lalr1-add-depender (propagate-to propagate-from)
  (if (oset-insert! propagate-to (item-look-ahead-dependers propagate-from))
      (dolist (gs (the list (oset-item-list
			     (the oset (item-look-aheads propagate-from)))))
	(lalr1-add-lookahead gs propagate-to))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Discover and propagate lalr(1) look-aheads among members of lr(0)
;;; collection.

;;; This algorithm for propagating lalr(1) lookaheads is a straightforward
;;; recursive version of the algorithm sketched in section 6.9 of the (older)
;;; dragon book "Principles of Compiler Design" by A.V. Aho and J.D Ullman.
;;; The major drawback of this algorithm is that it may be somewhat wasteful
;;; of space.  With modern address spaces who cares?
;;; Basically, it crawls around on the lr(0) item sets and as it goes,
;;; it discovers both lookaheads which are "spontaneously" generated for
;;; an item set, and item sets to whom lookaheads propagate.  The doubly
;;; recursive way of implementing this is similar to the method used
;;; in calculating first sets in first.l

;;; (New) the names are getting a bit confusing here.  This function transforms
;;; the data structure *lr0-item-sets* from being the lr(0) collection to
;;; the lalr(1) collection.

;; the following is heavily optimized in the inner loop, and therefore hardly 
;; intelligible.  For reference look at the original Scheme program at the
;; end of this file.

(declaim (special *LR0-START-STATE-INDEX*))

(defun lalr1-do-lookaheads ()
  ;; Introduce a "dummy" terminal symbol which is used as a hack in
  ;; lookahead calculations.
  (let ((dummy-g-symbol (new-g-symbol "dummy" -1))
	(lr0-item-sets-item-list (oset-item-list (the oset *lr0-item-sets*)))
	(sad-list (list nil)))		; efficiency hack
    ;; The dummy symbol is terminal and must be in its own first set.
    (oset-insert! dummy-g-symbol (g-symbol-first-set dummy-g-symbol))
    ;; Map over all the kernel items.
    (dolist (item-set lr0-item-sets-item-list)
      (declare (type item-set item-set))
      (let* ((kernel (item-set-kernel item-set))
	     (index (item-set-index item-set))
	     (item-set-goto-map (item-set-goto-map item-set))
	     (goto-map-odf (oset-order-fn item-set-goto-map))
	     (goto-map-item-list (oset-item-list item-set-goto-map)))
	(declare (fixnum index))
	(dolist (kernel-item (the list (oset-item-list (the oset kernel))))
	  ;; Special case: the end symbol is a lookahead for the start
	  ;; production.
	  (if (= *lr0-start-state-index* index)
	      ;; There had better only be one item in this set!
	      (lalr1-add-lookahead *the-end-g-symbol* kernel-item))
	  ;; Here we use the hack in dragon 6.9 (fig 6.20) of using lr(1)
	  ;; closure with a dummy grammar symbol to discover propagated
	  ;; and spontaneous lookaheads for a lr(0) kernel item.  The
	  ;; funny-closure-items are in J' of the figure.
	  (dolist (funny-closure-item
		    ;; The set of "funny" closure items. J'.
		    (the list (oset-item-list
			       (the oset (single-item-closure-1
					  (copy-lr0-item kernel-item)
					  dummy-g-symbol)))))
	    (declare (type item funny-closure-item))
	    (block funny-closure-item
	      (let ((funny-closure-item-look-aheads
		     (item-look-aheads funny-closure-item)))
		(when (oset-empty? funny-closure-item-look-aheads)
		  (return-from funny-closure-item nil))
		(let* ((production (item-production funny-closure-item))
		       (production-length (production-length production))
		       (item-after-dot (item-after-dot funny-closure-item)))
		  (declare (fixnum production-length item-after-dot)
			   (type production production))
		  (when (= production-length item-after-dot)
		      (return-from funny-closure-item nil))
		  (let* ((goto-item-proto (make-item
					   :production production
					   :after-dot (1+ item-after-dot)))
			 (set (item-set-kernel
			       (cdr (or (progn
					  ;; instead of CONSing we reuse SAD-LIST
					  (setf (car (the CONS sad-list))
						(nth item-after-dot
						     (the list (rhs production))))
					  (dolist (item goto-map-item-list)
					    (when (eq 'equal
						      (funcall goto-map-odf
							       sad-list item))
					      (return item))))
					(error "Failed to find the goto set")))))
			 (odf (oset-order-fn set))
			 ;; Here we go to some expense to locate the goto set
			 ;; for an item.
			 ;; These should be pre-computed and cached instead.
			 (goto-item
			  (dolist (item (oset-item-list set)
				   (error "Failed to find goto item"))
			    (when (eq 'equal
				      (funcall odf goto-item-proto item))
			      (return item)))))
		    (dolist (lookahead
			      (oset-item-list
			       (the oset funny-closure-item-look-aheads)))
		      (if (eq lookahead dummy-g-symbol)
			  ;; Discovered lookahead propagation.
			  (lalr1-add-depender goto-item kernel-item)
			;; Discovered lookahead.
			(lalr1-add-lookahead lookahead goto-item))))))))))
      (princ "."))
    ;; NEW STUFF HERE: 1-27-88
    (terpri)
    (dolist (item-set lr0-item-sets-item-list)
      (declare (type item-set item-set))
      (closure-1! (item-set-closure item-set))
      (princ "."))))


;;; This should be primitive, and not insert if not there.
;;; Third arg is error msg
;;; result is eq to member of the set.

(defun oset-find (element set)
  (let ((odf (oset-order-fn set)))
    (dolist (item (oset-item-list set))
      (when (eq 'equal (funcall odf element item))
	(return item)))))

(defun copy-lr0-item (i)
  (make-item :production (item-production i)
             :after-dot (item-after-dot i)))

;;;    Do all needed to generate parse tables starting with a lisp syntax
;;;    grammar. (doesn't write out a table file)

(defun lalr1-tables-from-grammar (file-name &rest args)
  (apply #'load-grammar file-name args)
  (calculate-empty-string-derivers)
  (calculate-first-sets)
  (calculate-follow-sets)
  (make-lr0-collection)
  (lalr1-do-lookaheads)
  (build-parse-tables t)
  file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Original Scheme Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||
(define (lalr1-do-lookaheads)
    ;; Introduce a "dummy" terminal symbol which is used as a hack in
    ;; lookahead calculations.
    (let ((dummy-g-symbol (new-g-symbol "dummy" -1)))
      ;; The dummy symbol is terminal and must be in its own first set.
      (oset-insert! dummy-g-symbol (g-symbol-first-set dummy-g-symbol))
      ;; Map over all the kernel items.
      (oset-for-each
       (lambda (item-set)
	 (oset-for-each
	  (lambda (kernel-item)
	    ;; Special case: the end symbol is a lookahead for the start
	    ;; production.
	    (if (equal? lr0-start-state-index (item-set-index item-set))
		;; There had better only be one item in this set!
		(lalr1-add-lookahead the-end-g-symbol kernel-item))

	    ;; Here we use the hack in dragon 6.9 (fig 6.20) of using lr(1)
	    ;; closure with a dummy grammar symbol to discover propagated
	    ;; and spontaneous lookaheads for a lr(0) kernel item.  The
	    ;; funny-closure-items are in J' of the figure.

	    (oset-for-each
	     (lambda (funny-closure-item)
	       (if 
		   (not (oset-empty? (item-look-aheads funny-closure-item)))
		   (begin
		    (let ((goto-item-proto (advance-dot funny-closure-item)))
		      (if goto-item-proto
			  (begin
			   ;; Here we go to some expense to locate the goto set
			   ;; for an item.
			   ;; These should be pre-computed and cached instead.
			   (let ((goto-item
				  (oset-find
				   goto-item-proto
				   (item-set-kernel
				    (find-goto-set
				     item-set
				     (symbol-after-dot funny-closure-item)))
				   "internal error - failed to find goto item")))
			     (oset-for-each
			      (lambda (lookahead)
				(if (eq? lookahead dummy-g-symbol)
				    ;; Discovered lookahead propagation.
				    (lalr1-add-depender goto-item kernel-item)
				  ;; Discovered lookahead.
				  (lalr1-add-lookahead lookahead goto-item)))
			      (item-look-aheads funny-closure-item)))))))))
	     ;; The set of "funny" closure items. J'.
	     (single-item-closure-1 (copy-lr0-item kernel-item)
				    dummy-g-symbol)))
	  (item-set-kernel item-set))
	 (display "."))
       lr0-item-sets))

  ;; NEW STUFF HERE: 1-27-88
  (newline)
  (oset-for-each
   (lambda (item-set)
     (closure-1! (item-set-closure item-set))
     (display "."))
   lr0-item-sets
   ))
||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test
#||
(set-working-direct *ZEBU-test-directory*)
(lalr1-tables-from-grammar "ex6_2.zb")

(progn
  (lalr1-tables-from-grammar "ex4.zb")
  (princ "symbols: ") (terpri)
  (cruise-symbols-2)
  (princ "productions: ") (terpri)
  (print-productions)
  (princ "lr0 item sets: ") (terpri)
  (print-collection nil)
  (princ "lalr(1) tables: ") (terpri)
  (cruise-parse-tables)
  )
||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               End of zebu-lalr1.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
