; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         zebu-lr0-sets.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Fri Apr 23 10:00:40 1993 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-lr0-sets.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-lr0-sets.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.


;;; This defines the representation for sets of items, and
;;; computes the canonical lr(0) collection of sets of items.
;;; It currently leaves the closures lying around on the sets
;;; of items, they could be flushed just after they are used.
;;; It gets hold of the grammar via the symbol 'augmented start
;;; and the application of g-symbol-own-productions to symbols.
;;; The grammar should have been previously internalized
;;; using load-grammar.

(in-package "ZEBU")

(defvar *lr0-item-set-count*)
(defvar *lr0-item-sets*)
(defvar *lr0-start-state-index*)
(declaim (fixnum *lr0-start-state-index*))

;;; A type for sets of items.
;;; The kernel will be a o-set of items, the closure might be
;;; an o-set, or might be null if we are trying to save space.
;;; goto-map will be a oset of pairs whose cars are grammar symbols
;;; and whose cdrs are item-sets.

(defstruct (item-set (:print-function
		      (lambda (item-set stream depth)
			(declare (ignore depth))
			(item-set-print-kernel item-set nil stream))))
  index
  kernel
  (closure ())
  goto-map)

(defun item-set-print-kernel (item-set closure-too? &optional (stream t))
  (oset-for-each
   #'(lambda (item)
       (item-print item stream) (terpri stream))
   (if closure-too?
       (item-set-get-closure! item-set)
     (item-set-kernel item-set))))

(declaim (inline goto-map-order-function item-set-order-function
	    new-item-set))

(defun goto-map-order-function (a b)
  (g-symbol-order-function (car (the cons a)) (car (the cons b))))

(defun new-item-set (kernel)
  (make-item-set :kernel kernel
		 :goto-map (make-oset
			    :order-fn #'goto-map-order-function)))


;;; Item sets can be identified by looking at their kernels, so:
(defun item-set-order-function (a b)
  (declare (type item-set a b))
  ;; (oset-order-function (item-set-kernel a) (item-set-kernel b))
  ;; expand call for efficiency
  (let* ((oset-a (item-set-kernel a))
	 (oset-b (item-set-kernel b))
	 (odf (oset-order-fn oset-a)))
    (labels ((oset-order-aux (ilista ilistb)
	       (if (null ilista)
		   'equal
		 (let ((item-order
			(funcall odf
				 (car (the cons ilista))
				 (car (the cons ilistb)))))
		   (if (eq 'equal item-order)
		       (oset-order-aux
			(cdr (the cons ilista)) (cdr (the cons ilistb)))
		     item-order)))))
      (if (eq odf (oset-order-fn oset-b))
	  (let ((a-card (oset-cardinality oset-a))
		(b-card (oset-cardinality oset-b)))
	    (declare (fixnum a-card b-card))
	    (if (< a-card b-card)
		'correct-order
	      (if (= a-card b-card)
		  ;; same cardinality, same type, so march down the lists...
		  (oset-order-aux (oset-item-list oset-a)
				  (oset-item-list oset-b))
		'wrong-order)))
	(error "incompatible types of sets: oset-order-function")))))

;;; Result is an oset of item-sets which comprise the canonical
;;; lr(0) sets of items.

(defun make-lr0-collection ()
  (let* ((lr0-set (make-oset :order-fn #'item-set-order-function))
	 (start-prod (car (g-symbol-own-productions
			   *augmented-start-g-symbol*)))
	 (initial-kernel
	  (make-oset
	   :item-list   (list (new-item start-prod))
	   :order-fn    #'item-order-function
	   :cardinality 1)))
    (let ((initial-state (new-item-set initial-kernel)))
      (lr0-insert-item-set! initial-state lr0-set)
      (setf *lr0-item-set-count* 0)
      (dolist (is (oset-item-list lr0-set))
	(setf (item-set-index is) (post-inc *lr0-item-set-count*)))
      (setf *lr0-start-state-index* (item-set-index initial-state))
      (format t "~S item sets~%" *lr0-item-set-count*) 
      (setf *lr0-item-sets* lr0-set)
      '())))

;----------------------------------------------------------------------------;
; lr0-insert-item-set!
;---------------------
; item-set should be of that type.
; Collection should be an o-set of item-sets.
; Returns a pointer to the item set in the collection.

(defun lr0-insert-item-set! (item-set collection)
  (multiple-value-bind (inserted? the-item)
      (oset-insert-2! item-set collection)
    (when inserted?			; item wasn't already there
      (let ((item-set-goto-map (item-set-goto-map item-set)))
	(princ ".")
	(dolist (subset (oset-select-subsets
			 (item-set-get-closure! item-set)
			 #'symbol-after-dot))
	  (declare (type oset subset))
	  ;; (assert (typep subset 'oset))
	  ;; subset is an oset of items with same after dot
	  (let ((subset-item-list (oset-item-list subset)))
	    (when subset-item-list
	      (let ((goto-set (make-oset :order-fn #'item-order-function)))
		(dolist (item subset-item-list)
		  (let ((next (advance-dot item)))
		    (if next (oset-insert! next goto-set))))
		(unless (oset-empty? goto-set)
		  (oset-insert!
		   (cons (symbol-after-dot (car subset-item-list))
			 (lr0-insert-item-set! (new-item-set goto-set)
					       collection))
		   item-set-goto-map))))))))
    the-item))

;;; Returns the oset of items which is the closure of the item
;;; set, calculating it if need be from the kernel.
;;; Caches the closure in the closure slot.
(defun item-set-get-closure! (item-set)
  (or (item-set-closure item-set)
      (setf (item-set-closure item-set) (closure (item-set-kernel item-set)))))


;;; This isn't used in the current implementation: Sep 13, 1989.
#||
(defun item-set-flush-closure (item-set)
  (setf (item-set-closure item-set) '()))

;; inline expanded in lr0-insert-item-set!
;;; Subset is an oset of items which all have the same after dot symbol.
;;; Result is an oset of items.
;;; Gives back an empty set if the dots are all the way to the right
;;; in the input set.

(defun goto (subset)
  (let ((result (make-oset :order-fn #'item-order-function)))
    (dolist (item (oset-item-list subset) result)
      (let ((next (advance-dot item)))
	(if next (oset-insert! next result))))))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test:
#||
(load-grammar "ex1.zb")
(make-lr0-collection)
(print-collection nil)
(print-collection t)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           End of zebu-lr0-sets.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
