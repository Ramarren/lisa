; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-package.l
; Description:  package definition
; Author:       Joachim H. Laubsch
; Created:      13-Nov-91
; Modified:     Thu Mar  7 09:13:58 1996 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/package.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1991, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: package.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "CL-USER")
(provide "zebu-package")

#+LUCID					; while not up tp CLtL2
(eval-when (compile load eval)
  (defmacro LCL::DECLAIM (decl-spec) `(proclaim ',decl-spec)))

#+LUCID
(defpackage "PSGRAPH"
    (:use "LUCID-COMMON-LISP"))

#-LUCID
(defpackage "PSGRAPH"
    (:use "COMMON-LISP"))

(defpackage "ZEBU"
    (:nicknames "ZB")
    #+LUCID (:use "LISP" "LUCID-COMMON-LISP")
    #+LUCID (:import-from "SYSTEM" "*KEYWORD-PACKAGE*")
    #+LUCID (:import-from "LCL" "DECLAIM")
    (:import-from "PSGRAPH" PSGRAPH::PSGRAPH)
    #+MCL   (:use "COMMON-LISP" "CCL")
    #+KCL   (:use "LISP")
    #+ALLEGRO (:use "COMMON-LISP" "EXCL")
    
    (:import-from "CL-USER" CL-USER::*ZEBU-DIRECTORY*
		  CL-USER::*ZEBU-binary-directory*)
    (:export "*COMMENT-BRACKETS*" "*COMMENT-START*" "*PRESERVE-CASE*"
	     "*CASE-SENSITIVE*"
	     "*DISALLOW-PACKAGES*" "*STRING-DELIMITER*"
	     "*SYMBOL-DELIMITER*"
	     "*IDENTIFIER-START-CHARS*" "*IDENTIFIER-CONTINUE-CHARS*"
	     "*ALLOW-CONFLICTS*" "*WARN-CONFLICTS*" 
	     "*CURRENT-GRAMMAR*" "*GENERATE-DOMAIN*"
	     "*ZEBU-VERSION*"
	     "CATEGORIZE" "END-OF-TOKENS-CATEGORY"
	     "COMPILE-LALR1-GRAMMAR" "COMPILE-SLR-GRAMMAR"
	     "DEBUG-PARSER"
	     "DEFRULE" "FILE-PARSER" "FIND-GRAMMAR" "IDENTITY*"
	     "IDENTIFIERP"
	     "KB-DOMAIN" "KB-DOMAIN-P" "KB-TYPE-NAME-P"
	     "KB-SEQUENCE" "KB-SEQUENCE-P" "*KB-SEQUENCE-SEPARATOR*"
	     "MAKE-KB-SEQUENCE" "KB-SEQUENCE-FIRST" "KB-SEQUENCE-REST"
	     "KB-DEF-SLOT-TYPE" "KB-SET-VALUED-SLOT-P"
	     "KB-SLOT-TYPE" "KB-SLOTS" "KB-SUPERTYPE" "KB-SUBTYPES"
	     "KB-LEGAL-SLOT-P"
             "KB-TREE-ATTRIBUTES" "DEFINE-TREE-ATTRIBUTES" "DEF-TREE-ATTRIBUTES"
	     "PREORDER-TRANSFORM" "POSTORDER-TRANSFORM"
	     "KIDS" "FOR-EACH-KID" "FOR-EACH-KID!"
	     "FOR-EACH-DESCENDANT" 
	     "KB-COPY" "KB-EQUAL" "KB-COMPARE"
	     "LIST-PARSER" "LR-PARSE" "PRINT-ACTIONS" "READ-PARSER"
	     "COMPILE-FROM-COMMAND-LINE"
	     "EMPTY-SEQ" "SEQ-CONS" "EMPTY-SET" "SET-CONS"
	     "K-4-3" "K-2-1" "K-2-2" "K-3-2" "CONS-1-3" "CONS-2-3"
	     "NUMBER" "STRING" "IDENTIFIER"
	     "SHOW-KB-HIERARCHY"
	     "ZEBU" "ZEBU-COMPILER" "ZEBU-COMPILE-FILE" "ZEBU-LOAD-FILE"
	     "ZEBU-RR" "ZEBU-TOP"
	     )
    #-LUCID
    (:import-from "CL-USER"
		  CL-USER::*LOAD-SOURCE-PATHNAME-TYPES*
		  CL-USER::*LOAD-BINARY-PATHNAME-TYPES*))

(in-package "ZB")
(declaim (special *COMMENT-BRACKETS* *COMMENT-START* *PRESERVE-CASE*
	          *CASE-SENSITIVE* *DISALLOW-PACKAGES* *STRING-DELIMITER*
	          *SYMBOL-DELIMITER* *IDENTIFIER-START-CHARS*
	          *IDENTIFIER-CONTINUE-CHARS*
	          *ALLOW-CONFLICTS* *WARN-CONFLICTS*
	          *CURRENT-GRAMMAR* *GENERATE-DOMAIN*
	          *ZEBU-DIRECTORY*
	          ))

#-LUCID
(declaim (special *LOAD-SOURCE-PATHNAME-TYPES*
                  *LOAD-BINARY-PATHNAME-TYPES*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            End of zebu-package.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
