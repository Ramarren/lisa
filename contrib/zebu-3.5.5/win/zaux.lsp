; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-aux.lisp
; Description:  Functions and structures common to compiler and driver
; Author:       Joachim H. Laubsch
; Created:      11-Oct-90
; Modified:     Wed Dec  9 12:22:24 1998 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/zaux.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zaux.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;  7-Apr-92 (Joachim H. Laubsch)
;  many efficiency improvements throughout based on using Lucid's monitor
;  facility.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
(provide "zebu-aux")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-LUCID (declaim (special *load-source-pathname-types* 
                          *load-binary-pathname-types*))
#+(or MCL Allegro CLISP)
(setq *load-source-pathname-types* '("lisp" NIL)
      *load-binary-pathname-types* '("fasl"))

#+(and :SUN :LUCID)
(setq *load-binary-pathname-types* '("sbin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Global Variables (shared by runtime system and compiler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-LUCID 
(defvar *KEYWORD-PACKAGE* (find-package "KEYWORD"))

(defvar *generate-domain* t
  "If true while Zebu compiling a grammar, generate the hierarchy
otherwise the domain-hierarchy is written by the user.")

(defvar *ZEBU-PACKAGE* (find-package "ZEBU"))

(defvar *open-categories* '("IDENTIFIER" "NUMBER" "STRING"))

(declaim (special *NULL-Grammar*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Functions common to runtime and compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list->vector (l)
  (let* ((len (length l))
	 (v (make-sequence 'vector len)))
    (declare (vector v))
    (dotimes (i len v)
      (setf (svref v i) (pop l)))))

(deftype IDENTIFIER  () '(and symbol (not null)))

(defun identifierp (x)
  (typep x 'IDENTIFIER))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Lexical analysis (regex) Run/Compile time data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Declare the global variables for storing the paren index list.
;;;
(defvar *regex-groups* (make-array 10))
(defvar *regex-groupings* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     External representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *zb-rules*)                     ; alist of rule-names and zb-rule structs

(defstruct zb-rule
  -name
  -productions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Internal Representation of Productions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; About the internal representation of productions:
;;;  production-index:  (0 .. Number of productions - 1)
;;;  lhs:               a g-symbol
;;;  rhs:               a list of g-symbols
;;;  production-length: the length of rhs

(defstruct (production (:conc-name nil))
  lhs
  rhs
  production-index
  production-length)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   check the first form of a grammar file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this applies to a .zb as well as a .tab file

(declaim (special *compiler-grammar* *identifier-continue-chars*
	          *identifier-start-chars*))

(defun check-grammar-options (options filename compiling
				      &aux g-name compiler?)
  ;; FILENAME is of type path
  ;; check the list of options for plausibility
  ;; on package conflict, Nil is returned to catch point: read-grammar-options
  ;; we must then read the options again with *package* set correctly
  (unless (and (listp options) (not (null options)))
    (error "~S is not a valid Options List for a Zebu grammar!" options))
  (flet ((wrng-make-grammar-arglist (key)
	   (error "~S is not a defined keyword for make-grammar." key)))
    (do ((gg options (cddr gg))) ((null gg))
      (let ((key (car gg)) (val (cadr gg)))
	(if (keywordp key)
	    (case key
	      (:NAME    (setq g-name val))
	      (:PACKAGE
	       (let ((p (find-package val)))
		 (if p
		     (progn
		       (use-package "ZEBU" p)
		       (unless (eq *package* p)
			 (setq *package* p)
			 (throw 'read-grammar-options nil)))
		   (error
		    "Package ~s should be defined before ~:[loading~;compiling~] ~S"
		    val compiling filename))))
	      (:GRAMMAR (let ((g (find-grammar val)))
			  (setq compiler? t)
			  (if g
			      (setq *compiler-grammar* g)
			    (warn "Grammar ~S is not loaded" val))))
	      (:IDENTIFIER-CONTINUE-CHARS
	       (setf *identifier-continue-chars* val))
	      (:IDENTIFIER-START-CHARS
	       (setf *identifier-start-chars* val))
	      ((:STRING-DELIMITER :SYMBOL-DELIMITER :FILE :DOMAIN
				  :LEX-CATS :WHITE-SPACE :DOMAIN-FILE
				  :INTERN-IDENTIFIER :CASE-SENSITIVE))
	      (t (wrng-make-grammar-arglist key)))
	  (wrng-make-grammar-arglist key))))
    (unless g-name
      (setq g-name (pathname-name filename)
	    options (list* ':NAME g-name options)))
    (when (and compiling (not compiler?))
      (warn "Compiling with :GRAMMAR \"null-grammar\".
To use the meta grammar use: :GRAMMAR \"zebu-mg\" in options list!")
      (setq *compiler-grammar* *NULL-Grammar*))
    (when compiling
      (setq options (list* ':FILE (namestring filename) options)))
    options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      The Root of the Domain Hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (kb-domain (:constructor nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Internal representation of the domain hierarchy as a tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (type-tree-node
	     (:print-function
		  (lambda (item stream level)
		    (declare (ignore level))
		    (format stream "[[~s]]"
			    (type-tree-node--label item)))))
  -label
  -subtypes
  -supertype				; back link
  -slots
  )

(defvar *domain-type-hierarchy*)	; a backlinked tree
(defvar *domain-HT* (make-hash-table))	; a dictionary label --> node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Map Domain def into Hashtable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (special *domain-HT* *open-categories*))
(defvar *lex-cats* nil)
;----------------------------------------------------------------------------;
; prepare-domain
;---------------
; convert a domain D (as read from a grammar file) into the tree representation
; 
(defun prepare-domain (domain)
  (clrhash *domain-HT*)
  (let* ((top (new-domain-node ':TOP nil nil)))
    (setf *domain-type-hierarchy* top
	  (type-tree-node--subtypes top)
	  (list*
	   (new-domain-node 'kb-sequence top '(first rest))
	   (new-domain-node 'kb-domain top '())
	   (nconc (mapcar #'(lambda (s)
			      (new-domain-node (intern s) top nil))
			  *open-categories*)
		  (mapcar #'(lambda (c) (new-domain-node (car c) top nil))
			  *lex-cats*))))
    (when domain
      (add-to-domain domain top)
      domain)))

(defun add-to-domain (node point)
  (if (consp node)
      (let* ((label (car node))
	     (slots (cadr (member ':slots node)))
	     (new-point (new-domain-node label point slots)))
	(push new-point (type-tree-node--subtypes point))
	(do ((args (cdr node) (cddr args)))
	    ((null args))
	  (when (eq (car args) ':subtype)
	    (add-to-domain (cadr args) new-point))))
    (let ((new-point (new-domain-node node point nil)))
      (push new-point (type-tree-node--subtypes point)))))

(defun new-domain-node (label supertype slots)
  (let ((new (make-type-tree-node
	      :-label label :-supertype supertype :-slots slots)))
    (setf (gethash label *domain-HT*) new)))

#||
(prepare-domain '(cl-user::arith-exp
		  :subtype (cl-user::factor :slots (-value))
		  :subtype (cl-user::*-op   :slots (-arg1 -arg2))
		  :subtype (cl-user::+-op   :slots (-arg1 -arg2))
		  :subtype (cl-user::expression :slots (-value))))
||#

(defun def-kb-domain-type (type super slots)
  (let*  ((super-nd (or (gethash super *domain-HT*)
			(new-domain-node
			 super (gethash ':top *domain-HT*) '())))	
	  (type-nd (or (gethash type *domain-HT*)
		       (new-domain-node type super-nd slots))))
    (pushnew type-nd (type-tree-node--subtypes super-nd))
    type-nd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           End of zebu-aux.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
