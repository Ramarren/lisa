; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         zebu-tables.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Mon Apr 11 14:11:29 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/tables.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: tables.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.
(in-package "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; On the representation of parsing tables:
;;;
;;; Action function is an array, indexed by the state number,
;;; of functions of grammar symbols represented as osets of
;;; 3 element lists containing a g-symbol index, the character
;;; s, r, or a for shift reduce or accept, and an integer encoding the
;;; next state, or production index as appropriate.
;;;
;;; Goto for non-terminals will be represented by a parallel array
;;; of osets of pairs whose cars are g-symbol indices, and whose
;;; cdrs are state indices.

(defvar *action-array*)
(defvar *goto-array*)
(declaim (type vector *action-array* *goto-array*))

;;; An oset order function for parse table entries.
(defun integer-function-order-function (a b)
  (integer-order-function (car (the cons a)) (car (the cons b))))

;;; Build the description of the state machine which is the lr-parser.
;;; The *lr0-item-sets* correspond to the states of the parser machine.

(defun build-parse-tables (doing-lalr1)
  (setf *action-array* (make-sequence 'vector *lr0-item-set-count*))
  (setf *goto-array* (make-sequence 'vector *lr0-item-set-count*))
  (dotimes (i *lr0-item-set-count*)
    (setf (svref (the vector *action-array*) i)
	  (make-oset :order-fn #'integer-function-order-function))
    (setf (svref (the vector *goto-array*) i)
	  (make-oset :order-fn #'integer-function-order-function)))
  (oset-for-each
   #'(lambda (item-set)
       (oset-for-each
	#'(lambda (goto-elt)	     
	    ;; Car of goto-elt is g-sym, cdr is item-set.
	    (if (g-symbol-non-terminal? (car goto-elt))
		(oset-insert! (cons (g-symbol-index (car goto-elt))
				    (item-set-index (cdr goto-elt)))
			      (svref (the vector *goto-array*)
				    (item-set-index item-set)))
	      (parse-table-insert! (g-symbol-index (car goto-elt))
				   :s
				   (item-set-index (cdr goto-elt))
				   item-set)))
	(item-set-goto-map item-set))
       (oset-for-each
	#'(lambda (closure-item)
	    ;; Could these be kernel items?
	    (if (dot-at-right-end? closure-item)
		(let* ((closure-item-production (item-production closure-item))
		       (lhs-closure-item-production (lhs closure-item-production)))
		  (if (eq *augmented-start-g-symbol* lhs-closure-item-production)
		      (parse-table-insert! (g-symbol-index *the-end-g-symbol*)
					   :a 0 item-set) ; accept, bogus 0
		    (oset-for-each
		     #'(lambda (gs)
			 (parse-table-insert!
			  (g-symbol-index gs)
			  :r
			  (production-index closure-item-production)
			  item-set))
		     ;; Here is the only difference between slr and lalr1
		     ;; (in the table construction phase).
		     (if doing-lalr1
			 (item-look-aheads closure-item)
		       (g-symbol-follow-set lhs-closure-item-production)))))))
	(item-set-get-closure! item-set))
       )
   *lr0-item-sets*))


;;; An auxillary function for adding an entry to a parse table.
;;; A simple feature allows the system to be used with some 
;;; ambiguous grammars:  if the variable *allow-conflicts* it true,
;;; then when a conflict arises at table construction time, simply
;;; prefer the action which is already in the tables.  
;;; This feature works for the "dangling else" problem.

(defvar *allow-conflicts* t)
(declaim (special *warn-conflicts*))

(defun parse-table-insert! (g-sym-index action-key index item-set)
  (let ((to-insert (list g-sym-index action-key index)))
    (multiple-value-bind (inserted? the-item)    
	(oset-insert-2! to-insert
			(svref *action-array* (item-set-index item-set)))
      (unless inserted?
	(when *warn-conflicts*
	  (warn "ACTION CONFLICT!!!-- state: ~S~%old entry: ~S  new entry: ~S~%~
                 Continuing to build tables despite conflicts.~%~
                 Will prefer old entry: ~S"
		(item-set-index item-set) the-item to-insert the-item))
	(unless *allow-conflicts* (error "ACTION CONFLICT"))))))

(declaim (inline get-print-name))
(defun get-print-name (index)
  (g-symbol-name (svref (the vector *symbol-array*) index)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               End of zebu-tables.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
