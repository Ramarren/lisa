; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-oset.lisp
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      14-Nov-90
; Modified:     Tue Aug  2 15:03:39 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-oset.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-oset.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.

(in-package "ZEBU")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Ordered Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A simple ordered set facility.  Items kept in these sets must
;;; have an order function: these are supplied for integers and
;;; osets themselves.  Items are kept in sorted lists, smallest
;;; first.  Could be re-done with binary search trees.
;;; See integer-order-function for how order functions are supposed to
;;; work.

;;; Constructor will default to make a set that orders integers.

(defstruct (oset (:copier nil)
		 )
  (item-list     '() :type list)
  (order-fn      #'integer-order-function)
  (cardinality   0   :type fixnum))

(declaim (inline oset-empty?))
(defun oset-empty? (oset) (null (oset-item-list oset)))

;;; Example of how the order function is supposed to work.

(declaim (inline integer-order-function))
(defun integer-order-function (a b)
  (declare (fixnum a b))
  (cond ((< a b) 'correct-order)
	((> a b) 'wrong-order)
	(T 'equal)))

;;; Destructively insert an item into a set
;;; Returns the item if it wasn't there already, else NIL.
(defun oset-insert! (item set)
  ;; Returns  NIL if nothing is inserted or T if item was inserted
  ;; otherwise like oset-insert-2!
  (declare (type oset set))
  (let ((ilist (oset-item-list set)))
    (if (null ilist)
	(progn (setf (oset-item-list set) (list item)
		     (oset-cardinality set) 1)
	       t)
      (let ((odf (oset-order-fn set))
	    order)
	(cond ((eq 'correct-order
		   (setq order (funcall odf item (car (the cons ilist)))))
	       (setf (oset-item-list set) (cons item ilist))
	       (incf (oset-cardinality set))
	       t)
	      ((eq 'equal order) nil)	; item already there
	      (T ;; Ilist isn't null, and item goes somewhere after
	         ;; the car of ilist.
	       (do ((ilist ilist ilist-cdr)
		    (ilist-cdr (cdr ilist) (cdr ilist-cdr)))
		   ((null ilist-cdr)
		    (setf (cdr (the cons ilist)) (list item))
		    (incf (oset-cardinality set))
		    t)
		 (let ((ilist-cdr1 (car (the cons ilist-cdr))))
		   (when (eq 'correct-order
			     (setq order (funcall odf item ilist-cdr1)))
		     (setf (cdr (the cons ilist)) (cons item ilist-cdr))
		     (incf (oset-cardinality set))
		     (return-from oset-insert! t))
		   (when (eq 'equal order) ; already there
		     (return-from oset-insert! nil))))))))))

;;; Returns two values: (1) NIL if nothing is inserted ot T if item was
;;; inserted, and (2) a pointer to the item either found or inserted
;;; into the set (so is eq to a member of the set).

(defun oset-insert-2! (item set)
  (declare (type oset set))
  (let ((ilist (oset-item-list set)))
    (if (null ilist)
	(progn (setf (oset-item-list set) (list item)
		     (oset-cardinality set) 1)
	       (values t item))
      (let ((odf (oset-order-fn set))
	    (ilist-hd (car (the cons ilist)))
	    order)
	(cond ((eq 'correct-order
		   (setq order (funcall odf item ilist-hd)))
	       (setf (oset-item-list set) (cons item ilist))
	       (incf (oset-cardinality set))
	       (values t item))
	      ((eq 'equal order) (values nil ilist-hd))
	      ;; item already there
	      (T ;; Ilist isn't null, and item goes somewhere after
	         ;; the car of ilist.
	       (do ((ilist ilist ilist-cdr) (ilist-cdr (cdr ilist) (cdr ilist-cdr)))
		   ((null ilist-cdr)
		    (setf (cdr (the cons ilist)) (list item))
		    (incf (oset-cardinality set))
		    (values t item))
		 (let ((ilist-cdr1 (car (the cons ilist-cdr))))
		   (when (eq 'correct-order
			     (setq order (funcall odf item ilist-cdr1)))
		     (setf (cdr (the cons ilist)) (cons item ilist-cdr))
		     (incf (oset-cardinality set))
		     (return-from oset-insert-2! (values t item)))
		   (when (eq 'equal order) ; already there
		     (return-from oset-insert-2! (values nil ilist-cdr1)))))))))))


;;; Insert a list of items into an oset. returns the SET.
(declaim (inline oset-insert-list!))
(defun oset-insert-list! (list oset)
  (dolist (x list oset) (oset-insert! x oset)))

;;; It's easy to define a generic order function on osets if they
;;; have the same order function
;;; making for easy osets of osets.

(defun oset-order-function (oset-a oset-b &aux (odf (oset-order-fn oset-a)))
  (declare (type oset oset-a oset-b))
  (labels ((oset-order-aux (ilista ilistb)
	     (if (null ilista)
		 'equal
	       (let ((item-order (funcall odf (car ilista) (car ilistb))))
		 (if (eq 'equal item-order)
		     (oset-order-aux (cdr ilista) (cdr ilistb))
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
      (error "incompatible types of sets: oset-order-function"))))

; (declaim (inline oset-comparable?))
; (defun oset-comparable? (oseta osetb)
;        (eq 'equal (oset-order-function oseta osetb)))

;----------------------------------------------------------------------------;
; oset-select-subsets
;--------------------
;;; Yields a list of disjoint subsets whose union is the set.  For
;;; each subset the value of selection-fn applied to the members is
;;; the same in the sense of eqv.
;;; partition set according to selection-fn

(defun oset-select-subsets (set selection-fn)
  (let ((r-ilist (oset-item-list set))
	(alist   '())
	(odf     (oset-order-fn set)))
    (dolist (item r-ilist)
      (let* ((key (funcall selection-fn item))
	     (found-association (assoc key alist :test #'eql)))
	(if found-association 
	    (setf (cdr found-association)
		  (cons item (cdr found-association)))
	  (push (cons key (list item)) alist))))
    (do ((alist-tl alist (cdr alist-tl)))
	((null alist-tl) alist)
      (let ((items (cdar (the cons alist-tl))))
	(setf (car alist-tl) (make-oset :item-list (nreverse items)
					:cardinality (length items)
					:order-fn odf))))))

(declaim (inline oset-for-each oset-memq oset-copy oset-union oset-empty!))
(defun oset-for-each (procedure set)
  (declare (type oset set))
  (dolist (x (oset-item-list set)) (funcall procedure x)))

(defun oset-memq (elt set)
  (member elt (oset-item-list (the oset set))))

(defun oset-copy (oset)
  (declare (type oset oset))
  (make-oset
   :item-list (copy-list (oset-item-list oset))
   :order-fn (oset-order-fn oset)
   :cardinality (oset-cardinality oset)))

(defun oset-union (oset1 oset2)
  (declare (type oset oset1 oset2))
  #||
  (assert (eql (oset-order-fn oset1) (oset-order-fn oset2))
	  ()
	  "Mismatched order functions in oset union.")
  (if (> (oset-cardinality oset1) (oset-cardinality oset2))
      (oset-insert-list! (oset-item-list oset2)
			 (oset-copy oset1))
    (oset-insert-list! (oset-item-list oset1)
		       (oset-copy oset2)))
  ||#
  (oset-insert-list! (oset-item-list oset1)
		     (oset-copy oset2)))
		
(defun oset-delete (item oset)
  (declare (type oset oset))
  (let ((item-list (oset-item-list oset)))
    (if (member item item-list)
	(make-oset :item-list (delete item item-list)
		   :cardinality (1- (oset-cardinality oset))
		   :order-fn (oset-order-fn oset))
      oset)))			

(defun oset-empty! (oset)
  (declare (type oset oset))
  (setf (oset-cardinality oset) 0
	(oset-item-list oset) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 LR(1) items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lr(1) items.
;;; These are going to be represented by structs:
;;; after-dot is an integer which indexes the symbol in the
;;; production which follows the dot
;;; that comes after the dot.
;;;
;;; look-aheads is an oset of grammar symbols.
;;; The item data structure
;;; essentially stands for the set of lr(1) items which are the same
;;; except for each having one lookahead symbol from the set look-aheads.
;;;
;;; look-ahead-dependers is an oset of items to whom
;;; lalr(1) lookaheads
;;; propagate from this item.

(defstruct (item (:print-function item-print))
  (production    nil)
  (after-dot     0 :type fixnum)
  (look-aheads   (make-oset :order-fn #'g-symbol-order-function))
  (look-ahead-dependers
                 (make-oset :order-fn #'item-order-function)))

;;; A handy predicate.
(declaim (inline dot-at-right-end?))

(defun dot-at-right-end? (item)
  (declare (type item item))
  (= (the fixnum (production-length (item-production item)))
     (the fixnum (item-after-dot item))))

;;; Get the symbol after the dot -- 'the-bogus-symbol if dot is flushright.
(defun symbol-after-dot (item)
  (declare (type item item))
  (let ((pr-after (nthcdr (the fixnum (item-after-dot item))
			  (the list (rhs (item-production item))))))
    (if pr-after
	(car pr-after)
      'the-bogus-symbol)))

;;; Make an item with the dot moved one to the right, or false if
;;; dot gets past the end.
;;; Since this is used during lr(0) set construction, it only
;;; deals with production and after-dot slots, the others
;;; are filled in as '() by default.
(defun advance-dot (item)
  (declare (type item item))
  (let ((production (item-production item))
	(item-after-dot (item-after-dot item)))
    (if (= (production-length production)
	   (the fixnum item-after-dot)) 
	nil
      (make-item :production production
		 :after-dot (1+ item-after-dot)))))

;;; Make an item which has the dot at the left end of the rhs.
(declaim (inline new-item))
(defun new-item (production)
  (make-item :production production))

;;; For osets of items:
;;; this is used during lr(0) sets of items construction.  Only the
;;; production and after dot fields are tested, since these characterize
;;; lr(0) items.

(defun item-order-function (ia ib)
  (declare (type item ia ib))
  (let ((production-index-a (production-index (item-production ia)))
	(production-index-b (production-index (item-production ib))))
    (declare (fixnum production-index-a production-index-b))
    (if (< production-index-a production-index-b)
	'correct-order
      (if (= production-index-a production-index-b)
	  (let ((iad (item-after-dot ia)) (ibd (item-after-dot ib)))
	    (declare (fixnum iad ibd))
	    (if (< iad ibd)
		'correct-order
	      (if (= iad ibd)
		  'equal
		'wrong-order)))
	'wrong-order))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test:

#||
 (integer-order-function 1 2)
 (setq fred (make-oset))
 (oset-item-list fred)
 (oset-insert! 3 fred)
 (oset-insert-2! 4 fred) 
 (oset-insert-list! '(5 6 7 7) fred)
 (oset-insert-list! '(10 11) fred)
 (oset-insert! 1100 fred)
 (setq ned (make-oset))
 (setq mary (make-oset :order-fn #'oset-order-function))
 (oset-insert! ned mary)
 (oset-insert! ned mary)
 (oset-insert! fred mary)
 (oset-insert! fred mary)
 (mapc #'oset-item-list (oset-item-list mary))
 (mapc #'oset-item-list  (oset-select-subsets fred #'(lambda (x) (> x 5))))
 (mapc #'oset-item-list  (oset-select-subsets fred #'evenp))
 (oset-for-each #'(lambda (x) (format t "~S " x)) fred)
 (oset-memq 5 fred)
 (oset-memq 99 fred)
 (setq freddy (oset-copy fred))
 (oset-item-list freddy)
 (setq al (car (oset-select-subsets fred #'evenp)))
 (setq hal (cadr (oset-select-subsets fred #'evenp)))
 (oset-item-list (oset-union al hal))
 (oset-item-list fred)
 (oset-item-list (oset-delete 1100 fred))
 (oset-empty! freddy)
 (oset-item-list freddy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test: "zebu-item"
#||
 (defun red ((new-item (car *productions*)))
 (item-print fred)
 (defvar ned (advance-dot fred))
 (item-print ned)
 (item-order-function ned ned)
 (item-order-function ned fred)
 (item-order-function fred ned)
 (symbol-after-dot fred)
 (dot-at-right-end? fred)
 (dot-at-right-end? ned))
||#

||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                End of zebu-oset.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
