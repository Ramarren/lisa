; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         zebu-closure.lisp
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Tue Aug  2 16:11:09 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/closure.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: closure.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Calculate the closure of an lr(0) set of items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun closure (I)
  (declare (type oset I))
  (let ((eset 
	 (make-oset :order-fn #'item-order-function)
	  ))
    ;; I is an oset of items.
    ;; This is non-destructive.
    ;; See Fig. 4.33 of Dragon
    (labels ((closure-insert-item! (item)
	       ;; Add an item to an oset of items. Add his pals too if he wasn't
	       ;; there already.
	       (when (oset-insert! item eset)
		 (unless (dot-at-right-end? item)
		   (dolist (production 
			     (the list 
				  (g-symbol-own-productions
				   (symbol-after-dot item)))
			    nil)
		     (let ((new (new-item production)))
		       (closure-insert-item! new)))
		   ))))
      (dolist (x (oset-item-list I)) (closure-insert-item! x))
      eset)))

#||
(defun closure (I)
  (declare (type oset I))
  (let ((eset (make-oset :order-fn #'item-order-function)))
    ;; I is an oset of items.
    ;; This is non-destructive.
    ;; See Fig. 4.33 of Dragon
    (labels ((closure-insert-item! (item)
	       ;; Add an item to an oset of items. Add his pals too if he wasn't
	       ;; there already.
	       (when (oset-insert! item eset)
		 (unless (dot-at-right-end? item)
		   (dolist (production (g-symbol-own-productions
					(symbol-after-dot item)))
		     (closure-insert-item! 
		      (the item (new-item production))))))))
      (dolist (x (oset-item-list I)) (closure-insert-item! x))
      eset)))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calculate the lr(1) closure of a set of lr(1) items.
;;; Currently, find the closure of a set of one lr(1) item.
;;;
;;; An lr(1) item data structure with a set of lookaheads
;;; actually stands for a set of lr(1) items which are the
;;; same except for each having one lookahead from the set.

(defun single-item-closure-1 (lr0-item look-ahead)
  (let ((eset (make-oset :order-fn #'item-order-function)))
    (closure-1-insert-item! lr0-item look-ahead eset)
    eset))


;;; Destructively take the lr(1) closure of an item set
;;; (actually an oset of items... not an item-set structure).
;;; Empty out the set and re-insert the contents with closures.

(defun closure-1! (item-set)
  (let ((item-list (oset-item-list item-set)))
    (setf (oset-item-list item-set) nil)
    (dolist (item item-list)
      (let ((the-look-aheads (item-look-aheads item)))
	(setf (item-look-aheads item) 
	      (make-oset :order-fn #'g-symbol-order-function))
	(dolist (look-ahead (oset-item-list the-look-aheads))
	  (closure-1-insert-item! item look-ahead item-set))))))

;----------------------------------------------------------------------------;
; closure-1-insert-item!
;-----------------------
; See Dragon Fig. 4.38
; 

(defun closure-1-insert-item! (lr0-item look-ahead item-set)
  (declare (type item lr0-item))
  (labels ((closure-1-insert-item-aux (lr0-item look-ahead)
	     (multiple-value-bind (item-not-there-already the-item)
		 (oset-insert-2! lr0-item item-set)
	       (when (or (oset-insert! look-ahead (item-look-aheads the-item))
			 item-not-there-already)
		 ;; Item wasn't already there with that lookahead
		 ;; so insert his buddies too.
		 (unless (dot-at-right-end? lr0-item)
		   (let* ((prod (item-production lr0-item))
			  (rhs  (rhs prod))
			  (after-dot-rhs
			   (nthcdr (the fixnum (item-after-dot lr0-item))
				   (the cons rhs)))
			  (gs-list (oset-item-list
				    (first-seq-1
				      ;; This gets the list corresponding to the
				      ;; part of the item beyond the symbol after
				      ;; the dot.
				      (cdr (the cons after-dot-rhs))
				      look-ahead))))
		     (dolist (prod (g-symbol-own-productions
				    (car (the cons after-dot-rhs))))
		       (dolist (gs gs-list)
			 (let ((new (new-item prod)))
			   (closure-1-insert-item-aux new gs))))))))))
    (closure-1-insert-item-aux lr0-item look-ahead)))

#|
(defun closure-1-insert-item! (lr0-item look-ahead item-set)
  (declare (type item lr0-item))
  (labels ((closure-1-insert-item-aux (lr0-item look-ahead)
	     (multiple-value-bind (item-not-there-already the-item)
		 (oset-insert-2! lr0-item item-set)
	       (when (or (oset-insert! look-ahead (item-look-aheads the-item))
			 item-not-there-already)
		 ;; Item wasn't already there with that lookahead
		 ;; so insert his buddies too.
		 (unless (dot-at-right-end? lr0-item)
		   (let* ((prod (item-production lr0-item))
			  (rhs  (rhs prod))
			  (after-dot-rhs
			   (nthcdr (the fixnum (item-after-dot lr0-item))
				   (the cons rhs)))
			  (gs-list (oset-item-list
				    (first-seq-1
				      ;; This gets the list corresponding to the
				      ;; part of the item beyond the symbol after
				      ;; the dot.
				      (cdr (the cons after-dot-rhs))
				      look-ahead))))
		     (dolist (prod (g-symbol-own-productions
				    (car (the cons after-dot-rhs))))
		       (dolist (gs gs-list)
			 (closure-1-insert-item-aux
			  (new-item prod) gs)))))))))
    (closure-1-insert-item-aux lr0-item look-ahead)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test:
#||
(set-working-directory *ZEBU-test-directory*)
(zb::load-grammar "ex1.zb")
(zb::compile-slr-grammar "ex1.zb")
(zebu-load-file "ex1.tab")
(calculate-empty-string-derivers)
(calculate-first-sets)
(setq f-item (new-item (car (reverse *productions*))))
(setq f-i-set (single-item-closure-1
	       f-item *the-end-g-symbol*))
(item-list-print (oset-item-list f-i-set))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              End of closure1.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
