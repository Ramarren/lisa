; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         dump.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Fri Mar  8 14:46:38 1996 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/dump.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: dump.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
; 16-Jul-91 (Joachim H. Laubsch)
;  to deal with multiple-grammars, begin a ".tab" file with *GRAMMAR-OPTIONS*
;  a keyworded arglist that can be passed to MAKE-GRAMMAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.

;;;
;;; Dump parsing tables and associated stuff into a file.
;;;
;;; The follwing stuff is dumped in parenthesized lists which a lisp reader
;;; should be able to read:
;;;
;;; A keyword argument list for the MAKE-GRAMMAR function.
;;; An ordered (by grammar symbol index) lexicon.
;;; A list of the indices of terminal grammar symbols.
;;; A list of production info, ordered by production index, of lists
;;;    containing the index of the lhs grammar symbol and the length
;;;    of the rhs of the production.
;;; A sparse list of lists representation of the action function
;;;    (eyball one and you'll get the idea...).
;;; A similar representation of the goto function.
;;; The index of the start state.
;;; The index of the end symbol.
;;; A list of the client lambda forms.

(in-package "ZEBU")
(declaim (special *ACTION-ARRAY* *GOTO-ARRAY* *LR0-START-STATE-INDEX*))

(defun dump-tables (grammar-file output-file)
  (macrolet ((delete! (item sequence)
	       `(delete ,item ,sequence :test #'equal)))
    (let ((*print-structure* t)
	  *print-pretty* *print-length* *print-level* *print-circle*
	  (filename (if output-file
			(pathname output-file)
		      (merge-pathnames
		       (make-pathname :type "tab")
		       grammar-file))))
      (format t "~%Dumping parse tables to ~A~%" filename)
      (with-open-file (port filename :if-does-not-exist :create
			    :if-exists :supersede
			    :direction :output)
	;; 1: Dump options
	(format port "~%~S" *grammar-options*)
	;; 2: Dump out an ordered lexicon.
	(let ((ln (length *g-symbol-alist*)))
	  (format port "~%#~S(" ln)
	  (dolist (pair (reverse *g-symbol-alist*))
	    (format port "~S " (car pair)))
	  (format port ")~%~%"))
	;; 3: Dump a list of the indices of terminal grammar symbols
	;; deal with some special cases... .
	(let ((gs-list (delete
			'()
			(delete!
			 *empty-string-g-symbol*
			 (delete!
			  *augmented-start-g-symbol*
			  (delete!
			   *the-end-g-symbol*
			   (mapcar #'(lambda (gs)
				       (unless (g-symbol-non-terminal? gs) gs))
				   (reverse *symbols*))))))))
	  (format port "~%#~S(" (length gs-list))
	  (dolist (gs gs-list)
	    (format port "~S " (g-symbol-index gs)))
	  (format port ")~%~%"))
	;; 4: productions
	;; For the lr parser, dump a list of info on the productions.
	;; The order of the list follows the productions indices in
	;; the parse tables.  Each element is a list of the index of
	;; the lhs grammar symbol and the length of the rhs of the production.
	(format port "#~S(" (length *productions*))
	(dolist (prod (reverse *productions*))
	  (format port "(~S . ~S)"
		  (g-symbol-index (lhs prod))
		  (production-length prod)))
	(format port ")~%")

	;; 5: Dump out a representation of the action function.
	(let ((aa-len (length (the vector *action-array*))))
	  (format port "~%#~S(" aa-len)
	  (dotimes (i aa-len)
	    (format port "~%~S" (oset-item-list (svref *action-array* i))))
	  (format port ")~%"))

	;; 6: Dump out a representation of the goto function for non-terminals
	(let ((ga-len (length (the vector *action-array*))))
	  (format port "~%#~S(" ga-len)
	  (dotimes (i (length *goto-array*))
	    (format port "~%(")
	    (dolist (item (oset-item-list (svref *goto-array* i)))
	      (format port "~S" item))
	    (format port ")"))
	  (format  port ")"))

	;; 7: Dump the index of the start state.
	(print *lr0-start-state-index* port)
	(terpri port) 

	;; 8: Dump the index of the end symbol.
	(print (g-symbol-index *the-end-g-symbol*) port)
	(terpri port)

	;; 9: Dump out a vector of the client lambdas
	(let (*print-pretty*)
          (format port "~%#~S(~{~S~%~})" 
                  (length *zb-rules*)
		  (setq *zb-rules* (nreverse *zb-rules*))))
	)
      filename)))

;; Set up some convenient ways to process grammars.

(defun compile-slr-grammar (grammar-file &rest args)
  (apply #'compile-zebu-grammar-aux
	 grammar-file
	 #'slr-tables-from-grammar
	 args))

(defun compile-lalr1-grammar (grammar-file &rest args)
  (apply #'compile-zebu-grammar-aux
	 grammar-file
	 #'lalr1-tables-from-grammar
	 args))

(declaim (special *compiler-grammar*))
(defun compile-zebu-grammar-aux
    (grammar-file compiler
		  &key
		  (output-file (merge-pathnames
				(make-pathname :type "tab")
				grammar-file))
		  (grammar *null-grammar*)
		  verbose
		  (compile-domain t))
  (let ((*compiler-grammar* grammar)
	(*package* *package*))
    (setq grammar-file (funcall compiler grammar-file :verbose verbose))
    (when (get-grammar-options-key ':PACKAGE)
      (setq *package* (find-package (get-grammar-options-key ':PACKAGE))))
    (let ((domain-file (dump-domain-file grammar-file verbose)))
      (when (and compile-domain domain-file)
	(compile-file
	 domain-file
	 :output-file (merge-pathnames
		       (make-pathname
                        :host	   (pathname-host domain-file) ;; Added by Henry
			:name      (pathname-name domain-file)
			:directory (pathname-directory output-file)
			:type      (car *load-binary-pathname-types*)))
	 )))
    (dump-tables grammar-file output-file)))

;;;;;;;;;;;;;
;;; test:
#||
(set-working-directory *ZEBU-test-directory*)
(compile-slr-grammar "ex1.zb")
(compile-slr-grammar "ex2.zb")

;; fails : not slr
(compile-slr-grammar "ex3.zb") 
;;(compile-slr-grammar "ex4.zb")

;; fails : not slr
(compile-slr-grammar "ex6-2.zb") 
(compile-lalr1-grammar "ex1.zb")
(compile-lalr1-grammar "ex2.zb")
(compile-lalr1-grammar "ex3.zb")
(compile-lalr1-grammar "ex4.zb")
(compile-lalr1-grammar "ex6-2.zb")

||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                End of dump.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
