; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-compile.lisp
; Description:  apply the grammar-compiler
; Author:       Joachim H. Laubsch
; Created:       6-Nov-90
; Modified:     Tue Aug  2 16:20:04 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-compile.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-compile.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
; 25-Apr-91 (Joachim H. Laubsch)
;  introduced *WARN-CONFLICTS* to shut up warnings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ZEBU")

;; whether warnings about action-conflicts are printed at compile time
(defvar *warn-conflicts* nil)
(defvar *compiler-grammar* *null-grammar*
  "The grammar that the Zebu Compiler uses when reading a grammar.
By default this is the Null-Grammar.")

(defun zebu-compile-file (grammar-file
			  &key (grammar *null-grammar*)
			  output-file
			  verbose
			  (compile-domain t))
  "Compiles the LALR(1) grammar in file GRAMMAR-FILE."
  (assert (probe-file (setq grammar-file
                        (merge-pathnames grammar-file
                                         (merge-pathnames
					  (make-pathname :type "zb")))))
	  (grammar-file)
	  "Cannot find grammar file: ~A" grammar-file)
  (setq output-file
	(let ((tab (make-pathname :type "tab")))
          (if output-file    
	      (merge-pathnames (pathname output-file) tab)
            (merge-pathnames tab grammar-file))))
  (when (probe-file output-file) (delete-file output-file))
  (format t "~%; Zebu Compiling (Version ~A)~%; ~S to ~S~%"
	  *zebu-version* grammar-file output-file)
  (let ((*warn-conflicts* verbose))
    (compile-lalr1-grammar grammar-file
			   :output-file output-file
			   :grammar grammar
			   :verbose verbose
			   :compile-domain compile-domain)))


;----------------------------------------------------------------------------;
; compile-from-command-line
;--------------------------
; call zebu-compile-file with a command-line-argument
; 
#+LUCID
(defun compile-from-command-line ()
  (let ((*default-pathname-defaults*
	 (make-pathname :directory
			(pathname-directory (working-directory))
			:type "zb"))
	(ifile (command-line-argument 1))
	ofile
	verbose
	compile-domain)
    (if (null ifile)
	(Warn "No input file specified!")
      (progn
	(do* ((a 2 (1+ a))
	      (arg (command-line-argument a) (command-line-argument a)))
	     ((null arg))
	  (cond ((equalp arg "-v") (setq verbose t))
		((equalp arg "-d") (setq compile-domain t))
		((equalp arg "-r") (load (command-line-argument (incf a))))
		((= a 2) (setq ofile arg))))
	(apply #'zebu-compile-file ifile
	       :verbose verbose
	       :compile-domain compile-domain
	       (when ofile
		 `(:output-file ,ofile)))))
    (terpri)
    (quit)))


;----------------------------------------------------------------------------;
; zebu-top
;---------
; interactive compiler invocation

(defun zebu-compile-top ()
  (format t "~&Enter the name of a Zebu Grammar file to compile: ")
  (let ((ifile (read-line t)))
    (zebu-compile-file ifile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            End of zebu-compile.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
