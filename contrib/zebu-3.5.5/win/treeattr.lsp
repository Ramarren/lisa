; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-tree-attributes.lisp
; Description:  Functions operating on abstract syntax trees
; Author:       Joachim H. Laubsch
; Created:      26-Feb-93
; Modified:     Wed Oct 12 21:26:14 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/treeattr.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
(require "zebu-kb-domain")
(require "zebu-mg-hierarchy")
(provide "zebu-tree-attributes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               tree attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plist implementation

(declaim (inline KB-TREE-ATTRIBUTES))
(defun KB-tree-attributes (class-name)
  (get (the symbol class-name) 'KB-TREE-ATTRIBUTES))
;----------------------------------------------------------------------------;
; define-tree-attributes
;-----------------------
; for each class enter the tree attributes in the form:
; ((<reader1> <reader2> ...) .  (<writer1> <writer2> ...))
; where <readeri> is the name of the accessor for slot i
;       <writeri> is a compiled function to set slot i

(defun define-tree-attributes (class slots)
  (let (writers)
    (dolist (slot slots)
      (let ((def `(lambda (x y)
		   (declare (type ,class x))
		   (setf (,slot x) y))))
	(push
	 (compile nil def)
      	 writers)))
    (setf (get (the symbol class) 'KB-TREE-ATTRIBUTES)
	  (cons slots (nreverse writers))) ))

;; The reason for this macro is that then the compiler does
;; not need to be loaded when a file is loaded which contains 
;; def-tree-attributes forms
#||
(defmacro def-tree-attributes (class &rest slots)
  (check-type class symbol)
  (let (writers setters)
    (dolist (slot slots)
      (check-type slot symbol)
      (let* ((setter (intern (format nil "SET-~a" slot)))
	     (def `(defun ,setter (x y)
		    (declare (type ,class x))
		    (setf (,slot x) y))))
	(push def writers)
	(push setter setters)))
    `(progn
      ,@writers
      (setf (get ',class 'KB-TREE-ATTRIBUTES)
       (cons
	',slots
	(mapcar #'(lambda (setter) (symbol-function setter))
		    ',(nreverse setters)))))))

;; avoid duplicate definitions
(defmacro def-tree-attributes (class &rest slots)
  (check-type class symbol)
  (let (writers setters)
    (dolist (slot slots)
      (check-type slot symbol)
      (let ((setter (intern (format nil "SET-~a" slot))))
	(unless (fboundp setter)
	  (push `(defun ,setter (x y)
		  (declare (type ,class x))
		  (setf (,slot x) y))
		writers))
	(push setter setters)))
    `(progn
      (eval-when (compile eval) ,@writers)
      (setf (get ',class 'KB-TREE-ATTRIBUTES)
       (cons
	',slots
	(mapcar #'(lambda (setter) 
		     (symbol-function setter))
		',(nreverse setters)))))))

||#

(defmacro def-tree-attributes (class &rest slots)
  (check-type class symbol)
  (flet ((wrong-slotdescr (d)
	   (error "Tree attribute ~s not a symbol~%or of the form (<symbol> :set)"
		  d)))
    (let (writers setters set-valued-slots)
      (dolist (slotdescr slots)
	(let (slot setter)
	  (typecase slotdescr
	    (symbol (setf slot slotdescr))
	    (cons   (setf slot (first slotdescr))
		    (push slotdescr set-valued-slots))
	    (t (wrong-slotdescr slotdescr)))
	  (setf setter (intern (format nil "SET-~a" slot)))
	  (unless (fboundp setter)
	    (push `(defun ,setter (x y)
		    (declare (type ,class x))
		    (setf (,slot x) y))
		  writers))
	  (push setter setters)))
      `(progn
	(eval-when (compile eval #+CLISP load) ,@writers)
	,@(mapcar #'(lambda (set-valued-slot)
		      (let ((type (second set-valued-slot)))
			(if (eq type :set)
			    `(zb::KB-def-slot-type 
			      ',(first set-valued-slot) :set)
			  (wrong-slotdescr set-valued-slot))))
		  set-valued-slots)      
	(setf (get ',class 'KB-TREE-ATTRIBUTES)
	 (cons
	  ',slots
	  (list . ,(mapcar #'(lambda (setter) `(function ,setter))
			   (nreverse setters)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashtable implementation 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||
(defvar *KB-TREE-ATTRIBUTES* (make-hash-table))
(declaim (type HASH-TABLE *KB-TREE-ATTRIBUTES*))

(declaim (inline KB-TREE-ATTRIBUTES))
(defun KB-TREE-ATTRIBUTES (class-name)
  (gethash class-name *KB-TREE-ATTRIBUTES*))

;----------------------------------------------------------------------------;
; define-tree-attributes
;-----------------------
; for each class enter the tree attributes in the form:
; ((<reader1> <reader2> ...) .  (<writer1> <writer2> ...))
; where <readeri> is the name of the accessor for slot i
;       <writeri> is a compiled function to set slot i

(declaim (inline KB-TREE-ATTRIBUTES))
(defun define-tree-attributes (class slots)
  (let (writers)
    (dolist (slot slots)
      (let ((def `(lambda (x y)
		   (declare (type ,class x))
		   (setf (,slot x) y))))
	(push 
	 (compile nil def)
	 writers)))
    (setf (gethash class *KB-TREE-ATTRIBUTES*)
	  (cons slots (nreverse writers))) ))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Set/Sequence Valued Slots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *KB-SLOT-types* (make-hash-table))
(declaim (type HASH-TABLE *KB-SLOT-types*))

(declaim (inline KB-set-valued-slot-p))
(defun KB-set-valued-slot-p (reader)
  (eq (gethash reader *KB-SLOT-types*) ':set))

(defun KB-def-slot-type (reader type)
  (setf (gethash reader *KB-SLOT-types*) type))

;----------------------------------------------------------------------------;
; kids
;-----
; collect all the kids of OBJECT which are in KB-Domain.
; if a kid is a SET or SEQUENCE of subnodes, include those which are
; in KB-Domain.

(defun kids (object &aux R)
  (declare (inline KB-TREE-ATTRIBUTES))
  (macrolet ((readers (x) `(the list (car (the cons ,x)))))
    (let ((ta (KB-tree-attributes (type-of object))))
      (when ta
	(dolist (reader (readers ta) R)
	  (declare (symbol reader))
	  (let ((kids (funcall (the function (symbol-function reader)) object)))
	    (cond ((consp kids)
		   (dolist (k (the list kids))
		     (when (KB-Domain-p k) (push k R))))
		  ((KB-Domain-p kids)
		   (push kids R))))))))
  )

;-----------------------------------------------------------------------------;
; subexpressions
;---------------
; 
; All immediate subexpressions of a KB-Domain-element
; anything not of type KB-Domain-element does not have components

(declaim (inline subexpressions))
(defun subexpressions (KB-Domain-element)
  (check-type KB-Domain-element KB-Domain)
  (kids KB-Domain-element))

;----------------------------------------------------------------------------;
; for-each-kid
;-------------
; iterate over all kids of NODE which are in KB-Domain, calling FUN.
; NODE must be of type KB-Domain.
; Returns nil

(defun for-each-kid (FUN NODE)
  (declare (type function fun))
  (macrolet ((readers (x) `(the list (car (the cons ,x)))))
    (if (KB-Domain-p NODE)		;  (subtypep typ 'KB-Domain)
	(let ((ta (KB-tree-attributes (type-of node))))
	  (when ta
	    (dolist (reader (readers ta))
	      (declare (symbol reader))
	      (let ((subnode (funcall (the function (symbol-function reader)) NODE)))
		(cond
		  ((CONSp subnode)	; value is a set or sequence
		   (dolist (kid (the list subnode))
		     (when (KB-Domain-p kid) (funcall FUN kid))))
		  ((KB-Domain-p subnode) (funcall fun subnode)))))))
      (error "Can't iterate over non KB-Domain object: ~S" NODE))))

(defun for-each-kid! (FUN NODE)
  ;; just like for-each-kid, but if FUN(kid) ~eq kid then replace kid
  ;; by the value of FUN(kid)
  ;; returns NODE
  (declare (type function fun))
  (declare (inline KB-TREE-ATTRIBUTES))
  (if (KB-Domain-p NODE)		;  (subtypep typ 'KB-Domain)
      (macrolet ((readers (x) `(the list (car (the cons ,x))))
		 (writers (x) `(the list (cdr (the cons ,x)))))
	(let ((ta (KB-tree-attributes (type-of node))))
	  (if (null ta)
	      NODE
	    ;; ta ((<reader1> <reader2> ...) (<writer1> <writer2> ...))
	    (do ((r (readers ta) (cdr r)) (w (writers ta) (cdr w)))
		((null r) NODE)
	      (let* ((reader (car (the cons r)))
		     (subnode (funcall (the function (symbol-function reader))
				       NODE)))
		(cond
		  ((CONSp subnode)	; value is a set or sequence
		   (do ((kids (the list subnode) (cdr kids)))
		       ((null kids))
		     (let ((kid (car (the cons kids))))
		       (if (KB-Domain-p kid)
			   (let ((newval (funcall FUN kid)))
			     (unless (eq kid newval)
			       (setf (car kids) newval)))))))
		  ((KB-Domain-p subnode)
		   (let ((vv (funcall fun subnode)))
		     (unless (eq vv subnode)
		       ;; (eval `(setf (,reader ,NODE) ',vv))
		       (funcall (the compiled-function (car w)) NODE vv))))))))))
    (error "Can't iterate over non KB-Domain object: ~S" NODE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             preorder-transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||
(defun preorder-transform (node funs)
  (check-type node KB-Domain)
  (check-type funs list)
  (macrolet ((readers (x) `(the list (car (the cons ,x))))
	     (writers (x) `(the list (cdr (the cons ,x))))
	     (mung-node (n) `(preorder-transform-aux (transform-node ,n))))
    (labels ((preorder-transform-aux (n)
	       (let ((ta (KB-tree-attributes (type-of n))))
		 (when (null ta)
		   (return-from preorder-transform-aux n))
		 (do ((r (readers ta) (cdr r)) (w (writers ta) (cdr w)))
		     ((null r) n)
		   (let* ((reader (car (the cons r)))
			  (subnode (funcall (the function (symbol-function reader))
					    n)))
		     (cond ((CONSp subnode) ; value is a set or sequence
			    (do ((kids (the list subnode) (cdr kids)))
				((null kids))
			      (let ((kid (car (the cons kids))))
				(when (KB-Domain-p kid)
				  (let ((newval (mung-node kid)))
				    (unless (eq kid newval)
				      (setf (car (the cons kids)) newval)))))))
			   ((KB-Domain-p subnode)
			    (let ((subnode1 (mung-node subnode)))
			      (unless (eq subnode1 subnode)
				(funcall (car (the cons w)) n subnode1)))))))))
	     (transform-node (n)
	       (let (fun-fired?)
		 (do ((funRest (the list funs))
		      (oldn (KB-copy n) (KB-copy n)))
		     ((null funRest) n)
		   (let ((fun (car funRest)))
		     ;; run each function to acquiescence
		     ;; each function returns 2 values, 
		     ;; (1) the new node
		     ;; (2) whether there was a change in this node
		     ;;     that may make it necessary for this function to run
		     ;;     again on the same node
		     ;; if a function had an effect --- fun-fired? = T  ---
		     ;; we start all over with all functions (except the current)
		     (loop do (multiple-value-bind (v change?)
				  (funcall (the Function fun) n)
				(if change?
				    (setq n v)
				  (if (eq n v)
				      (return n)
				    (setq n v)))
				(format t "~%;; ~S~%;; ~S~%;; --> ~S" fun oldn v)
				(setq fun-fired? t)))
		     (if fun-fired?
			 (setq funRest (remove fun funs)
			       fun-fired? nil)
		       (pop funRest)))))))
      (mung-node node))))
||#
(defun preorder-transform (node funs)
  (declare (inline KB-TREE-ATTRIBUTES))
  (check-type node KB-Domain) (check-type funs list)
  (macrolet ((readers (x) `(the list (car (the cons ,x))))
	     (writers (x) `(the list (cdr (the cons ,x))))
	     (mung-node (n) `(preorder-transform-aux (transform-node ,n))))
    (flet ((transform-node (n)
	     (let (fun-fired?)
	       (do ((funRest (the list funs)))
		   ((null funRest) n)
		 (let ((fun (car funRest)))
		   ;; run each function to acquiescence
		   ;; each function returns 2 values, 
		   ;; (1) the new node
		   ;; (2) whether there was a change in this node
		   ;;     that may make it necessary for this function to run
		   ;;     again on the same node
		   ;; if a function had an effect --- fun-fired? = T  ---
		   ;; we start all over with all functions (except the current)
		   (loop (multiple-value-bind (v change?)
			     (funcall (the Function fun) n)
			   (if change?
			       (setq n v)
			     (if (eq n v)
				 (return n)
			       (setq n v)))
			   ;; (format t "~%;; ~S~%;; ~S~%;; --> ~S" fun oldn v)
			   (setq fun-fired? t)))
		   (if fun-fired?
		       (setq funRest (remove fun funs)
			     fun-fired? nil)
		     (pop funRest)))))))
      (labels ((preorder-transform-aux (n)
		 (let ((ta (KB-tree-attributes (type-of n))))
		   (if (null ta)
		       n
		     (do ((r (readers ta) (cdr r))
			  (w (writers ta) (cdr w)))
			 ((null r) n)
		       (let* ((reader (car (the cons r)))
			      (subnode (funcall 
					(the function
					     (symbol-function reader))
					n)))
			 (cond ((CONSp subnode) ; value is a set or sequence
				(do ((kids (the list subnode) (cdr kids)))
				    ((null kids))
				  (let ((kid (car (the cons kids))))
				    (when (KB-Domain-p kid)
				      (let ((newval (mung-node kid)))
					(unless (eq kid newval)
					  (setf (car (the cons kids)) newval)))))))
			       ((KB-Domain-p subnode)
				(let ((subnode1 (mung-node subnode)))
				  (unless (eq subnode1 subnode)
				    (funcall (car (the cons w)) 
					     n subnode1)))))))))))
	(mung-node node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             postorder-transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; just like preorder, but descend down first to the leaves, and then
;; transform from bottom-up
(defun postorder-transform (node funs &optional (exhaustive nil))
  (declare (inline KB-TREE-ATTRIBUTES))
  (check-type node KB-Domain) (check-type funs list)
  (macrolet ((readers (x) `(the list (car (the cons ,x))))
	     (writers (x) `(the list (cdr (the cons ,x))))
	     ;; here is the difference to preorder: recurse first!
	     (mung-node (n)
	       `(transform-node (postorder-transform-aux ,n))))
    (flet ((transform-node (n)
	     (block transform-node
	       ;; (format t "~%transform-node: ~S" n)
	       (do ((funRest (the list funs)) rule-fired?)
		   ((null funRest) n)
		 (let ((fun (car funRest)))
		   ;; run each function to acquiescence
		   ;; each function returns 2 values, 
		   ;; (1) the new node
		   ;; (2) whether there was a change in this node
		   ;;     that may make it necessary for this function to run
		   ;;     again on the same node
		   ;; if a function had an effect --- fun-fired? = T  ---
		   ;; we start all over at the leaves
		   (loop (multiple-value-bind (v change?)
			     (funcall (the Function fun) n)
			   (if change?
			       (setq n v)
			     (if (eq n v)
				 (return nil)
			       (setq n v)))
			   (if exhaustive
			       (return-from transform-node
				 (values n t))
			     (setq rule-fired? t))))
		   (if rule-fired?
		       (setq funRest (remove fun funs)
			     rule-fired? nil)
		     (pop funRest))))))) 
      (labels ((postorder-transform-aux (n)
		 (let ((ta (KB-tree-attributes (type-of n))))
		   (if (null ta)
		       n
		     ;; (format t "~%postorder-transform: ~S" n)
		     (do ((r (readers ta) (cdr r)) (w (writers ta) (cdr w)))
			 ((null r) n)
		       (let* ((reader (car (the cons r)))
			      (subnode (funcall (the function
						     (symbol-function reader))
						n)))
			 (cond ((CONSp subnode) ; value is a set or sequence
				(do ((kids (the list subnode) (cdr kids)))
				    ((null kids))
				  (let ((kid (car (the cons kids))))
				    (when (KB-Domain-p kid)
				      (loop
				       (multiple-value-bind (newval rule-fired?)
					   (mung-node kid)
					 (if (eq kid newval)
					     (if rule-fired?
						 (if exhaustive
						     nil ; go on
						   (return nil))
					       (return nil))
					   (progn
					     (setf (car (the cons kids)) newval)
					     (setf kid newval)
					     (if exhaustive
						 nil ; go on
					       (return nil))))))))))
			       ((KB-Domain-p subnode)
				(loop
				 (multiple-value-bind (subnode1 rule-fired?)
				     (mung-node subnode)
				   (if (eq subnode1 subnode)
				       (if rule-fired?
					   (if exhaustive
					       nil ; go on
					     (return nil))
					 (return nil))
				     (progn
				       (funcall (car (the cons w)) n subnode1)
				       (setf subnode subnode1)
				       (if exhaustive
					   nil ; go on
					 (return nil))))))))))))))
	(loop (multiple-value-bind (new rule-fired?)
		  (mung-node node)
		(if exhaustive
		    (if (or rule-fired? (not (eq new node)))
			(setq node new)
		      (return new))
		  (return new))))))))

#||
(defun descendants (object)
  (let ((R (list object)))
    (dolist (kid (kids object) R)
      (nconc R (descendants kid)))))
||#
;; more efficiently:

;----------------------------------------------------------------------------;
; descendants
;------------
; 

(defun descendants (object &aux R)
  (declare (inline KB-TREE-ATTRIBUTES))
  (check-type object KB-Domain)    
  (macrolet ((readers (x) `(the list (car (the cons ,x)))))  
    (labels ((descendants-aux (object)
	       (let ((ta (KB-tree-attributes (type-of object))))
		 (when ta
		   (dolist (reader (readers ta))
		     (declare (symbol reader))
		     (let ((kids (funcall (the function (symbol-function reader))
					  object)))
		       (cond ((consp kids)
			      (dolist (k (the list kids))
				(push k R)
				(descendants-aux k)))
			     ((KB-Domain-p kids)
			      (push kids R)
			      (descendants-aux kids)))))))))
      (descendants-aux object)
      (nreverse (cons object R)))))

;----------------------------------------------------------------------------;
; for-each-descendant
;--------------------
; like for-each-kid
; Returns nil

(defun for-each-descendant (fn object)
  (declare (type function fn))
  (check-type object KB-Domain) 
  (macrolet ((readers (x) `(the list (car (the cons ,x)))))  
    (labels ((descendants-aux (object)
	       (let ((ta (KB-tree-attributes (type-of object))))
		 (when ta
		   (dolist (reader (readers ta))
		     (declare (symbol reader))
		     (let ((kids (funcall (the function (symbol-function reader))
					  object)))
		       (cond ((consp kids)
			      (dolist (k (the list kids))
				(funcall fn k)
				(descendants-aux k)))
			     ((KB-Domain-p kids)
			      (funcall fn kids)
			      (descendants-aux kids)))))))))
      (funcall fn object)
      (descendants-aux object))))

;----------------------------------------------------------------------------;
; KB-copy
;--------
; A copy function that walks down all the tree-attributes and copies
; unless called with :recursive-p Nil

#+LUCID
(defmacro %copy-structure (x)
    `(SYSTEM:copy-structure ,x))

#-LUCID
(defun %copy-structure (term)
  (let* ((ttype (type-of term))
	 (copy-fn (find-symbol (concatenate 
				'string "COPY-" (symbol-name ttype))
			       (symbol-package ttype))))
    (if (fboundp copy-fn)
	(funcall copy-fn term)
      (error "No COPY function defined for ~s:~a" term ttype)))) 

(defun KB-copy (term &optional (recursive-p t))
  (declare (inline KB-TREE-ATTRIBUTES))
  (macrolet ((readers (x) `(the list (car (the cons ,x))))
	     (writers (x) `(the list (cdr (the cons ,x)))))
    (labels ((KB-copy-aux (term)
	       (declare (type KB-Domain term))
	       (let ((new-term (%COPY-STRUCTURE term))
		     (ta (KB-tree-attributes (type-of term))))
		 (if (null ta)
		     new-term
		   (do ((r (readers ta) (cdr r)) (w (writers ta) (cdr w)))
		       ((null r) new-term)
		     (let* ((reader (car (the cons r)))
			    (writer (car (the cons w)))
			    (subnode (funcall (the function
						   (symbol-function
						    (the symbol reader)))
					      new-term)))
		       (cond ((CONSp subnode) ; value is a set or sequence 
			      (let ((newsubnode
				     (copy-list (the list subnode))))
				(funcall (the compiled-function writer)
					 new-term newsubnode)
				(do ((nrest newsubnode (cdr nrest)))
				    ((null nrest))
				  (let ((kid (car (the cons nrest))))
				    (when (KB-Domain-p kid)
				      (setf (car (the cons nrest))
					    (KB-copy-aux kid)))))))
			     ((KB-Domain-p subnode)
			      (funcall (the compiled-function writer)
				       new-term
				       (KB-copy-aux subnode))))))))))
      (if recursive-p
	  (KB-copy-aux term)
	(%COPY-STRUCTURE term)))))

#||
;; test
(setq $a (eval (READ-PARSER "walk(agt : John ) ")))
(setq $aa (KB-copy $a))
;;
(car (KB-tree-attributes (type-of $a)))
;; (ATOMIC-WFF--PREDICATE ATOMIC-WFF--ROLE-ARGUMENT-PAIRS)
(eq (ATOMIC-WFF--PREDICATE $a) (ATOMIC-WFF--PREDICATE $aa)) ; NIL
(equal (ATOMIC-WFF--PREDICATE $a) (ATOMIC-WFF--PREDICATE $aa)) ; Nil
(kb-equal (ATOMIC-WFF--PREDICATE $a)  (ATOMIC-WFF--PREDICATE $aa)) ; T
;; note: equalp does recursive descent on structures
(equalp (ATOMIC-WFF--PREDICATE $a) (ATOMIC-WFF--PREDICATE $aa))	; T

(setq $b (eval (READ-PARSER "and{walk(agent: John) talk(agent: John)}")))
(type-of $b)
(car (KB-tree-attributes (type-of $b)))
(setq $bb  (KB-copy $b))

||#

;----------------------------------------------------------------------------;
; KB-equalp
;----------
; compares 2 objects of the KB-domain for equality. (something like term-equal?)
;  considers only tree-attributes as relevant
;  This is easier to extend for set-valued slots:

(defun KB-equal (a b)
  (declare (inline KB-TREE-ATTRIBUTES))
  (check-type a KB-domain)
  (check-type b KB-domain)
  ;; ignores implementation of constants
  (macrolet ((readers (x) `(the list (car (the cons ,x)))))
    (labels
	((KB-equal-aux (a b)
	   (block KB-equal-aux
	     (let ((a-typ (type-of a)) (b-typ (type-of b)))
	       (unless (equal a-typ b-typ) (return-from KB-equal-aux 'Nil))
	       (let ((ta (KB-tree-attributes a-typ)))
		 (or
		  (null ta)
		  (dolist (reader (readers ta) t)
		    (declare (symbol reader))
		    (let* ((reader-fn (symbol-function reader))
			   (a-subnode (funcall reader-fn a))
			   (b-subnode (funcall reader-fn b)))
		      (unless (eq a-subnode b-subnode)
			(unless (equal (type-of a-subnode) (type-of b-subnode))
			  (return-from KB-equal-aux 'Nil))
			(cond
			  ((CONSp a-subnode) ; value is a set or sequence
			   (if (= (the fixnum (length (the list a-subnode)))
				  (the fixnum (length (the list b-subnode))))
			       (if (KB-set-valued-slot-p reader)
				   ;; We have 2 sets to compare
				   ;; resort to this to avoid consing, see
				   ;; comment below:
				   (unless
				       (and (dolist (bb (the list b-subnode) t)
					      (unless (dolist (aa (the list a-subnode))
							(when (KB-equal-aux aa bb)
							  (return t)))
						(return nil)))
					    (dolist (aa (the list a-subnode) t)
					      (unless (dolist (bb (the list b-subnode))
							(when (KB-equal-aux aa bb)
							  (return t)))
						(return nil))))
				     (return-from KB-equal-aux 'Nil))
				 ;; We have two sequences to compare
				 ;; Their elements must be in KB-domain
				 (do ((arest a-subnode (cdr arest))
				      (brest b-subnode (cdr brest)))
				     ((atom arest) (eq arest brest))
				   (let ((aa (car (the cons arest)))
					 (bb (car (the cons brest))))
				     (unless (KB-equal-aux aa bb)
				       (return-from KB-equal-aux 'Nil)))))
			     (return-from KB-equal-aux 'Nil)))
			  ((KB-domain-p a-subnode)
			   (unless (KB-equal-aux a-subnode b-subnode)
			     (return-from KB-equal-aux 'Nil)))
			  ((symbolp a-subnode)
			   (unless (string-equal (symbol-name a-subnode)
						 (symbol-name b-subnode))
			     (return-from KB-equal-aux 'Nil)))
			  (T (unless (equal a-subnode b-subnode)
			       (return-from KB-equal-aux 'Nil)))))))))))))
      (or (equal a b)
	  (KB-equal-aux a b)))))

#||
(KB-equal (read-nll "DESKTOP-OBJECT(NAME: 'Orders--STR')")
	    (read-nll "DESKTOP-OBJECT(NAME: Orders--STR)"))
(KB-equal (read-nll "WORK(agent:+{'ABRAMS','BROWNE'})")
	    (read-nll "WORK(agent:+{ABRAMS,BROWNE})"))
(compile 'KB-equalp)
||#
;----------------------------------------------------------------------------;
; KB-compare
;-----------
;; the following is useful for testing

(defun KB-compare (a b &optional verbose 
		     &aux (msg "~% KB-compare ~S:~S ~%          = ~S:~S"))
  (declare (inline KB-TREE-ATTRIBUTES))
  (check-type a KB-domain)
  (check-type b KB-domain)
  (macrolet ((readers (x) `(the list (car (the cons ,x)))))
    (labels
	((KB-equal-aux (a b)
	   (block KB-equal-aux
	     (let ((a-typ (type-of a)) (b-typ (type-of b)))
	       (unless (equal a-typ b-typ) 
		 (when verbose (format t msg a a-typ b b-typ))
		 (return-from KB-equal-aux 'Nil))
	       (if (typep a 'KB-domain)
		   (let ((ta (KB-tree-attributes a-typ)))
		     (or
		      (null ta)
		      (dolist (reader (readers ta) t)
			(declare (symbol reader))
			(let* ((reader-fn (symbol-function reader))
			       (a-subnode (funcall reader-fn a))
			       (b-subnode (funcall reader-fn b)))
			  (when verbose
			    (format t msg
				    a-subnode (type-of a-subnode)
				    b-subnode (type-of b-subnode)))
			  (unless (eq a-subnode b-subnode)
			    (unless (equal (type-of a-subnode) (type-of b-subnode))
			      (return-from KB-equal-aux 'Nil))
			    (cond
			      ((CONSp a-subnode) ; value is a set or sequence
			       (if (= (the fixnum (length (the list a-subnode)))
				      (the fixnum (length (the list b-subnode))))
				   (if (KB-set-valued-slot-p reader)
				       ;; We have 2 sets to compare
				       ;; resort to this to avoid consing, see
				       ;; comment below:
				       (unless
					   (and (dolist (bb (the list b-subnode) t)
						  (unless (dolist (aa (the list a-subnode))
							    (when (KB-equal-aux aa bb)
							      (return t)))
						    (return nil)))
						(dolist (aa (the list a-subnode) t)
						  (unless (dolist (bb (the list b-subnode))
							    (when (KB-equal-aux aa bb)
							      (return t)))
						    (return nil))))
					 (return-from KB-equal-aux 'Nil))
				     ;; We have two sequences to compare
				     ;; Their elements must be in KB-domain
				     (do ((arest a-subnode (cdr arest))
					  (brest b-subnode (cdr brest)))
					 ((atom arest) (eq arest brest))
				       (let ((aa (car (the cons arest)))
					     (bb (car (the cons brest))))
					 (or (equal aa bb)
					     (KB-equal-aux aa bb)
					     (return-from KB-equal-aux 'Nil)))))
				 (return-from KB-equal-aux 'Nil)))
			      ((KB-domain-p a-subnode)
			       (unless (KB-equal-aux a-subnode b-subnode)
				 (return-from KB-equal-aux 'Nil)))
			      ((symbolp a-subnode)
			       (unless (string= (symbol-name a-subnode)
						(symbol-name b-subnode))
				 (return-from KB-equal-aux 'Nil)))
			      (T (unless (equal a-subnode b-subnode)
				   (return-from KB-equal-aux 'Nil)))))))))
		 (equal a b))))))
      (or (eq a b)
	  (KB-equal-aux a b)))))

#|| test
(KB-equal (make-Placeholder-Var :-Name 'u486)
	  (make-Placeholder-Var :-Name 'subject-nl-semantics))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       tree-attributes for kb-sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-tree-attributes kb-sequence
    kb-sequence-first kb-sequence-rest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate (define-tree-attributes ..) for zebu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Das nachfolgende kannst Du zur Generierung der Tree-Attributes
; verwenden. Prepare-Tree-Attributes kann auch zur Laufzeit die
; Attribute eintragen; wenn Du die Definitionen ins Domainfile
; uebernimmst, muesste Zebu define-tree-attributes immer kennen
; (zebu-kernel?).

;; diese hashtable ist fuer zebra wiederverwenbar (der gruene punkt)

(defparameter *local-accessor-hashtable* (make-hash-table :test #'equal))

(defun labelnode2accessor (label topnode)
  "Translates a label symbol and its topnode
   into a structure accessor (-predicate atomic-wff -> at-wff--pred)"
  (let* ((key (cons label topnode))
         (constr (gethash key *local-accessor-hashtable*)))
    (if constr
	constr
      (setf (gethash key *local-accessor-hashtable*)
	    (intern (concatenate 'string 
				 (symbol-name topnode) "-"
				 (symbol-name label)))))))

(defun prepare-tree-attributes (type &optional (output-only nil) (stream T))
  "sets kb-tree-attributes of type and all of its subtypes"
  (let ((slots (kb-slots type))
        (slot-funs nil))
    (dolist (item slots)
      (if (symbolp item)
          (push (labelnode2accessor item type) slot-funs)
        ;; else
        (push (labelnode2accessor (first item) type) slot-funs)))
    (when slot-funs
      (setq slot-funs (nreverse slot-funs))
      (if output-only
          (format stream "~S~%~%" 
                  `(define-tree-attributes ',type '(,@slot-funs)))
        ;; else
        (define-tree-attributes type slot-funs)))
    (dolist (item (kb-subtypes type))
      (prepare-tree-attributes item output-only stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     End of zebu-tree-attributes.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
