; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-kb-domain.lisp
; Description:  
; Author:       Joachim H. Laubsch
; Created:      19-Mar-93
; Modified:     Wed Aug  4 20:43:54 1993 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/kb-dom.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1993, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: kb-dom.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(IN-PACKAGE  "ZEBU")
(require "zebu-aux")
(provide "zebu-kb-domain")
;----------------------------------------------------------------------------;
; for-each-supertype
;-------------------
; Iterate fn over all supertypes of type. Type is the label of a
; type-tree-node in *domain-HT*
; Note that every type is its own supertype.

(defun for-each-supertype (fn type &optional errorp)
  (let ((node (gethash type *domain-HT*)))
    (labels ((doit (node)
	       (when (type-tree-node-p node)
		 (funcall fn node)
		 (doit (type-tree-node--supertype node)))))
      (if node
	  (doit node)
	(when errorp
	  (KB-type-error type))))))

;----------------------------------------------------------------------------;
; KB-legal-slot-p
;----------------
; Is slot-label a legal name of a slot of a type named TYPE?
; EXPORTED
(defun KB-legal-slot-p (type slot-label)
  (for-each-supertype #'(lambda (node)
			  (dolist (slot (type-tree-node--slots node))
			    (when (eq (if (consp slot)
					  (car slot)
					slot)
				      slot-label)
			      (return-from KB-legal-slot-p t))))
		      type
		      t))

;----------------------------------------------------------------------------;
; KB-slot-type
;-------------
; slot-label is a KB-legal-slot-p type
; if slot-label has a type restriction (<slot-label> <type-restriction>)
;    this restriction will be returned
; else :TOP wil be returned
; EXPORTED
(defun KB-slot-type (type slot-label)
  (for-each-supertype
   #'(lambda (node)
       (dolist (slot (type-tree-node--slots node))
	 (if (consp slot)
	     (when (eq (car slot) slot-label)
	       (return-from KB-slot-type (cadr slot)))
	   (when (eq slot slot-label)
	     (return-from KB-slot-type :TOP)))))
   type
   t)
  (error "~a is not a slot of ~a" slot-label type)
  )

;----------------------------------------------------------------------------;
; kb-slots
;---------
; given a type name, return its slots
; a slot may be a list (<slot-name> <type-name>)
; EXPORTED
(defun kb-slots (type &aux slots)
  (for-each-supertype
   #'(lambda (n)
       (setq slots (append (type-tree-node--slots n) slots)))
   type
   t)
  slots)

;----------------------------------------------------------------------------;
; kb-supertype
;-------------
; given a type name, return its supertype
; the top type is named :TOP and its supertype is :TOP
; EXPORTED
(defun kb-supertype (type)
  (let ((node (gethash type *domain-HT*)))
    (if node
	(if (eq *domain-type-hierarchy* node)
	    ':TOP
	  (type-tree-node--label
	   (type-tree-node--supertype node)))
      (KB-type-error type))))

;----------------------------------------------------------------------------;
; kb-subtypes
;------------
; given a type name, return a list of its subtypes
; EXPORTED
(defun kb-subtypes (type)
  (let ((node (gethash type *domain-HT*)))
    (if node
	(mapcar #'type-tree-node--label
		(type-tree-node--subtypes node))
       (KB-type-error type))))

(defun KB-type-error (type)
  (error "~a is not a KB-type" type))

(defun KB-type-name-p (item)
  ;; if ITEM is the name of a subtype of KB-domain
  (not (null (gethash item *domain-HT*))))

#|| test
(zb:compile-slr-grammar (merge-pathnames "arith-exp.zb"
					 user::*ZEBU-test-directory*)
			:output-file (merge-pathnames
				      "binary/arith-exp.tab"
				      user::*ZEBU-test-directory*)
			:grammar (find-grammar "zebu-mg"))
(zb:zebu-load-file (merge-pathnames "binary/arith-exp.tab"
				    user::*ZEBU-test-directory*))
(ds:load-system 'user::Zebu-rr)
(KB-slot-type 'user::Mult-op 'user::-arg1)
(kb-slots 'user::Plus-op)
(kb-slots 'user::Factor)
(kb-supertype 'user::Factor)
(kb-supertype 'user::ARITH-EXP)
(kb-supertype 'KB-DOMAIN)
(kb-supertype 'KB-SEQUENCE)
(kb-supertype ':TOP)

(kb-subtypes ':TOP)
(KB-type-name-p 'IDENTIFIER)
(KB-type-name-p 'KB-DOMAIN)
(KB-subtypes 'KB-DOMAIN)
(kb-subtypes 'user::ARITH-EXP)
(kb-subtypes 'user::+-OP)
(kb-slots    'user::+-OP)

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         End of zebu-kb-domain.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
