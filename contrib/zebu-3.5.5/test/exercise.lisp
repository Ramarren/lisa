; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         exercise.lisp
; Description:  
; Author:       Joachim H. Laubsch
; Created:      26-Mar-92
; Modified:     Wed Jan 13 13:41:01 1999 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/test/Attic/exercise.lisp,v 1.1 2000/10/12 02:39:45 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: exercise.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:45  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "CL-USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  pathnames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (boundp '*ZEBU-directory*)
  (setq *ZEBU-directory*
	(make-pathname :directory (butlast (pathname-directory *LOAD-TRUENAME*)))
	))

(setq *ZEBU-binary-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-directory*)
				    (list "binary"))))

(defparameter *ZEBU-test-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-directory*)
				    (list "test"))))

(defparameter *ZEBU-test-binary-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-test-directory*)
				    (list "binary"))))

(defparameter *ZEBU-compile-domain*
  #+(or :ALLEGRO :HARLEQUIN-PC-LISP) nil
  #-(or :ALLEGRO :HARLEQUIN-PC-LISP) t)

#+DEFSYS
(let ((*default-pathname-defaults* *ZEBU-directory*))
  (require "ZEBU-sys")
  (ds:compile-system 'Zebu-compiler)
  (ds:load-system 'Zebu-compiler)
  (ds:load-system 'Zebu-rr)
  (use-package (find-package "ZEBU")
               (find-package "CL-USER"))
)

#-DEFSYS
(let ((*default-pathname-defaults* *ZEBU-directory*))
  (load (make-pathname :name "ZEBU-init" :type "lisp"))
  (zb:zebu-compiler :compiled *ZEBU-compile-domain*)
  (zb:zebu-rr :compiled *ZEBU-compile-domain*)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ex1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+DEFSYS
(progn (ds:compile-module "ex1") (ds:load-module "ex1"))
#-DEFSYS
(progn
  (zebu-compile-file (merge-pathnames
                      (make-pathname :name "ex1" :type "zb") *ZEBU-test-directory*)
                     :output-file
                     (merge-pathnames
                      (make-pathname :name "ex1" :type "tab")
                      *ZEBU-test-binary-directory*)
		     :compile-domain *ZEBU-compile-domain*)
  
  (zb:zebu-load-file (merge-pathnames
                      (make-pathname :name "ex1" :type "tab")
                      *ZEBU-test-binary-directory*)))
(setq zebu:*current-grammar* (find-grammar "ex1"))
(format t "Grammar: ~S" zebu:*current-grammar*)

(let ((l '(1 "+" a foo bar)))
  (multiple-value-bind (v rest)
      (list-parser l :junk-allowed t)
    (unless (and (equal v '(+OP (EXPRESSION (TERM (FACTOR 1)))
			    (TERM (FACTOR A))))
		 (eq rest (nthcdr 3 l)))
      (warn "list-parser broken"))))

(handler-case (equal (list-parser '(1  "+" a ) )
		     (read-parser "1 + a"))
	      (error () 'ok)
	      (:no-error (&rest args) args))

(if (and 
     (equal (read-parser "1 + a")
	    '(+OP (EXPRESSION (TERM (FACTOR 1)))
	      (TERM (FACTOR A))))
     (equal (read-parser "1 + a") (read-parser "1 + a  dd" :junk-allowed t))
     (equal (list-parser '(1 "+" a foo bar)  :junk-allowed t)
	    (read-parser "1 + a  foo bar" :junk-allowed t))
     (equal (read-parser ".1 + 1/3") (read-parser "0.1 + 1/3"))
     (equal (read-parser "1 + a") (list-parser '(1 "+" a)))
     (equal (read-parser "1 + 1") (list-parser '(1 "+" 1)))
     (equal (read-parser "1 + x * y") (list-parser '(1 "+" x "*" y)))
     (equal (read-parser "(1 + x) * y") (list-parser '("(" 1 "+" x ")" "*" y))))
    (print 'ok)
  (warn "error parsing with grammar ex1")) 

(zb:FILE-PARSER (merge-pathnames "sample-ex1" *ZEBU-test-directory*) 
                :grammar (zb:find-grammar "ex1"))

(zebu::cruise-follow-sets)
(zebu::print-productions)

(compile-slr-grammar
 (merge-pathnames "ex1a.zb" *ZEBU-test-directory*)
 :output-file (merge-pathnames "ex1a.tab" *ZEBU-test-binary-directory*))
(zb:zebu-load-file (merge-pathnames "ex1a.tab" *ZEBU-test-binary-directory*))

(unless (zb:read-parser "" :grammar (find-grammar "ex1a"))
  (warn "error with grammar ex1a, given an empty string"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              meta-grammar test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (equal (zb::grammar-identifier-start-chars (zb:find-grammar "zebu-mg"))
	       "$-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (warn "zebu-mg"))

(zb:read-parser "Program := KB-domain: [(-stmts KB-Sequence)];"
		:grammar (zb:find-grammar "zebu-mg"))

(zb:read-parser "Program := [(-stmts KB-Sequence)];"
		:grammar (zb:find-grammar "zebu-mg"))

(zb:read-parser "Arith-exp := Kb-domain : [];"
		:grammar (zb:find-grammar "zebu-mg"))

(zb:read-parser "Factor    := Arith-exp : [(-value)];"
		:grammar (zb:find-grammar "zebu-mg"))

(zb:read-parser "Program --> \"begin\" Stmts \"end\"
                 { Program: [(-stmts Stmts)
                             (-label \"bar\")] } ;"
		:grammar (zb:find-grammar "zebu-mg"))

(unless (equalp
	 (zb:read-parser "Program --> \"begin\" Stmts \"end\"
                          { Program: [(-stmts Stmts)] } ;"
			 :grammar (zb:find-grammar "zebu-mg"))
	 '#S(ZEBU::ZB-RULE
	     -NAME PROGRAM
	     -PRODUCTIONS (#S(ZEBU::PRODUCTION-RHS
			      -SYNTAX ("begin" STMTS "end")
			      -SEMANTICS #S(ZEBU::FEAT-TERM
					    -TYPE PROGRAM
					    -SLOTS (#S(ZEBU::LABEL-VALUE-PAIR
						       -LABEL -STMTS
						       -VALUE STMTS)))
			      -BUILD-FN NIL))))
  (warn "zebu-mg 1"))

(with-open-file (s (merge-pathnames "arith.zb" *ZEBU-test-directory*))
  (read s)
  (zb::file-parser-aux s #'error t (zb:find-grammar "zebu-mg") t))


#+DEFSYS (ds:compile-module "arith")
#-DEFSYS 
(zebu-compile-file (merge-pathnames "arith" *ZEBU-test-directory*)
                   :output-file (merge-pathnames
                                 "arith" *ZEBU-test-binary-directory*)
		   :compile-domain *ZEBU-compile-domain*)

(compile-file 
 (merge-pathnames (make-pathname :name "ar-dom"
				 :type (car *load-source-pathname-types*))
		  *ZEBU-test-directory*)
 :output-file (merge-pathnames
	       "ar-dom" *ZEBU-test-binary-directory*))

(defun PRINT-FACTOR (item STREAM LEVEL)
  (FORMAT STREAM "~a" (factor--value item)))
#+DEFSYS 
(ds:load-module "arith")
#-DEFSYS
(zebu-load-file (merge-pathnames (make-pathname :name "arith" :type "tab")
                                 *ZEBU-test-binary-directory*))
(zebu::print-actions "arith")

(unless (and (equalp (list-parser '(ned "+" jed)
				  :grammar (zb:find-grammar "arith"))
		     (read-parser "ned + jed"
				  :grammar (zb:find-grammar "arith")))

	     (equalp (read-parser "(ned + jed) * 4"
				  :grammar (zb:find-grammar "arith"))
		     '#S(Mult-OP
			 -ARG1 #S(FACTOR
				  -VALUE #S(Plus-OP -ARG1 #S(FACTOR -VALUE NED)
						    -ARG2 #S(FACTOR -VALUE JED)))
			 -ARG2 #S(FACTOR -VALUE 4))))
  (warn "arith did not compile correctly"))

(defun print-factor (item stream level)
  (declare (ignore level))
  (let ((v (factor--value item)))
    (if (or (symbolp v) (numberp v))
	(format stream "~a" v)
      (format stream "(~a)" v))))

(unless (string= (with-output-to-string (s)
		   (prin1
		    (read-parser "(ned + jed) * 4"
				 :grammar (zb:find-grammar "arith"))
		    s))
		 "(NED + JED) * 4")
  (warn "printing for arith failed"))

;; mini-la
(let ((zebu:*allow-conflicts* t)
      (*generate-domain* t))
  (compile-slr-grammar 
   (merge-pathnames "mini-la.zb" *ZEBU-TEST-DIRECTORY*)
   :output-file (merge-pathnames "mini-la.tab" *ZEBU-TEST-BINARY-DIRECTORY*))
  )
(setq zebu:*current-grammar*
      (zebu-load-file (merge-pathnames "mini-la.tab" *ZEBU-TEST-BINARY-DIRECTORY*)))

(unless (typep (zb::read-parser "begin a end" :grammar (zb:find-grammar "mini-la"))
	       'program)
  (warn "failed to parse with mini-la: 1"))

(unless (typep (zb::read-parser "begin A; B ; C end"
				:grammar (zb:find-grammar "mini-la"))
	       'program)
  (warn "failed to parse with mini-la: 2"))

(unless (typep (zb::read-parser "begin A; begin B1; B2 end ; C end"
				:grammar (zb:find-grammar "mini-la"))
	       'program)
  (warn "failed to parse with mini-la: 3"))

(let ((s " begin F({1,2,4}) end"))
  (equal (format nil "~s" (zb::read-parser s))
	 s)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ex6_2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*))
  (zebu-compile-file "ex6_2.zb"
   :output-file (merge-pathnames "ex6_2.tab" *ZEBU-TEST-BINARY-DIRECTORY*)
   :compile-domain *ZEBU-compile-domain*)
  (setq zebu:*current-grammar*
      (zebu-load-file (merge-pathnames "ex6_2.tab" *ZEBU-TEST-BINARY-DIRECTORY*)))
  )
(unless (and (let ((zb:*preserve-case* t))
	       (string= (format nil "~s" (read-parser "foo = 0"))
			"foo = 0"))
	     (eq (type-of (read-parser "**foo = ***x")) 'ASSIGNMENT)
	     (equalp (ASSIGNMENT--lhs
		      (read-parser "**foo = ***x"))
		     (read-parser "**foo")))
  (warn "Grammar ex6_2 did not compile correctly"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ex2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+DEFSYS
(progn (compile-module "ex2") (load-module "ex2"))

#-DEFSYS
(progn
  (zebu-compile-file (merge-pathnames
                      (make-pathname :name "ex2" :type "zb") *ZEBU-test-directory*)
                     :output-file
                     (merge-pathnames
                      (make-pathname :name "ex2" :type "tab")
                      *ZEBU-test-binary-directory*)
		     :compile-domain *ZEBU-compile-domain*
		     :verbose t)
  
  (zb:zebu-load-file (merge-pathnames
                      (make-pathname :name "ex2" :type "tab")
                      *ZEBU-test-binary-directory*)))

(setq zebu:*current-grammar* (find-grammar "ex2"))
(format t "Grammar: ~S" zebu:*current-grammar*)

(zebu::print-productions)
(zebu::cruise-follow-sets)
(zebu::print-actions (zebu::grammar-name zebu:*current-grammar*))

(and
 (equal (read-parser "G") (list-parser '(G)))
 (equal (read-parser "(G)") (list-parser '( "(" G ")" )))
 (equal (read-parser "(((P)))") '((((P)))))
 (equal (read-parser "(F + 3 + 1)") '((F "+" 3 "+" 1)))
 (equal (read-parser "(F + 3 * (2 + 1))") '((F "+" 3 "*" (2 "+" 1))))
 (equal (read-parser "(F + 3) * (2 + 1)") '((F "+" 3) "*" (2 "+" 1)))
 (equal (read-parser "((F + 3) * 2) + 1") '(((F "+" 3) "*" 2) "+" 1))
 (equal (list-parser '(ned "+" "(" jed "*" fred ")"))
	'(NED "+" (JED "*" FRED)))
 (print 1))

(and 
 (let (zebu:*current-grammar*)
   (equal (read-parser "ned + jed"     :grammar (find-grammar "ex2"))
	  (list-parser '(ned "+" jed ) :grammar (find-grammar "ex2"))))

 (equal (read-parser "ned + jed"     :grammar (find-grammar "ex2"))
	(list-parser '(ned "+" jed ) :grammar (find-grammar "ex2")))
 (print 2))

#+DEFSYS
(progn (compile-module "ex3") (load-module "ex3"))
#-DEFSYS
(progn
  (zebu-compile-file (merge-pathnames
                      (make-pathname :name "ex3" :type "zb") *ZEBU-test-directory*)
                     :output-file
                     (merge-pathnames
                      (make-pathname :name "ex3" :type "tab")
                      *ZEBU-test-binary-directory*)
		     :compile-domain *ZEBU-compile-domain*)
  
  (zb:zebu-load-file (merge-pathnames
                      (make-pathname :name "ex3" :type "tab")
                      *ZEBU-test-binary-directory*)))

(and 
 (equal (read-parser "b"     :grammar (find-grammar "ex3")) "b")
 (null  (read-parser ""     :grammar (find-grammar "ex3")))
 (print 3))

#+DEFSYS
(progn
  (compile-module "useless")
  (load-module "useless")
  (setq zebu:*current-grammar* (zb:find-grammar "useless"))
  )

#+DEFSYS
(progn (compile-module "lr4-21") (load-module "lr4-21"))

#-DEFSYS
(progn
  (zebu-compile-file (merge-pathnames
                      (make-pathname :name "lr4-21" :type "zb") *ZEBU-test-directory*)
                     :output-file
                     (merge-pathnames
                      (make-pathname :name "lr4-21" :type "tab")
                      *ZEBU-test-binary-directory*)
		     :compile-domain *ZEBU-compile-domain*)
  
  (zb:zebu-load-file (merge-pathnames
                      (make-pathname :name "lr4-21" :type "tab")
                      *ZEBU-test-binary-directory*)))

(setq zebu:*current-grammar* (zb:find-grammar "lr4-21"))
(read-parser "foo = 0")
(read-parser "foo = x")

(read-parser "*foo = x")
(read-parser "*0 = x")
(read-parser "**foo = ***x")


(zb:zebu-load-file
 (compile-slr-grammar (merge-pathnames "ex4.40.zb" *ZEBU-TEST-DIRECTORY*)
		      :output-file (merge-pathnames
				    "ex4.40.tab" *ZEBU-TEST-BINARY-DIRECTORY*)))
; this should print warnings
;    The following non-terminals where defined but not used: D E 
(equal (zb:read-parser "b" :grammar (find-grammar "ex4.40"))
       "b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Propositional Calculus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((zebu:*allow-conflicts* t)
      (*generate-domain* t))
  (compile-lalr1-grammar 
   (merge-pathnames "pc1.zb" *ZEBU-TEST-DIRECTORY*)
   :output-file (merge-pathnames "pc1.tab" *ZEBU-TEST-BINARY-DIRECTORY*)
   :verbose t)
  (load (merge-pathnames "pc1-dom.lisp" *ZEBU-TEST-DIRECTORY*))
  (load (merge-pathnames "pc1-p.lisp" *ZEBU-TEST-DIRECTORY*))
  (zebu-load-file (merge-pathnames "pc1.tab" *ZEBU-TEST-BINARY-DIRECTORY*))
  (setq zebu:*current-grammar* (zb:find-grammar "pc1"))
  )

(unless (and
	 (eq (type-of (read-parser "P")) 'PROPOSITIONAL-VARIABLE)
	 (type-of (read-parser "P and Q"))
	 (string= (format nil "~s" (read-parser "P and Q"))
		  "P and Q"))
  (warn "pc1 didn't compile correctly"))

(read-parser "P and Q and R")
(read-parser "P and Q or R and S")
(read-parser "(P and Q) or R and S")
(read-parser "P and (Q or R) and S")
(string= (format nil "~s" (read-parser "P(a: 1 b:S)"))
	 "P(A: 1 B: S)")
(let ((zb:*preserve-case* t))
  (unless (string= (format nil "~s" (read-parser "P(a: 1 b:S)"))
		   "P(a: 1 b: S)")
    (warn "Printing with grammar pc1 failed")))

(let ((zb:*preserve-case* nil))
  (unless (string= (format nil "~s" (read-parser "walks(agent: John)"))
		   "WALKS(AGENT: JOHN)")
    (warn "Printing with grammar pc1 failed")))

(zebu::print-actions "pc1")
(zebu::print-productions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                dangling else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((zebu:*allow-conflicts* t) (zebu:*warn-conflicts* t))
  (zebu-load-file
   (compile-lalr1-grammar 
    (merge-pathnames "dangelse.zb" *ZEBU-TEST-DIRECTORY*)
    :output-file (merge-pathnames "dangelse.tab"
				  *ZEBU-TEST-BINARY-DIRECTORY*))))

(unless (equal (read-parser "if f then if g then h else i" 
			    :grammar (find-grammar "dangelse"))
	       '("if" F "then" ("if" G "then" H "else" I)))
  (warn "error in dangelse grammar"))

(defpackage "ZEBU-TEST"
    #-LUCID (:use "COMMON-LISP")
    #+LUCID (:use "LUCID-COMMON-LISP")
    )

#+DEFSYS
(progn (compile-module "pc2") (load-module "pc2")
       )

#-DEFSYS
(zebu-load-file
 (compile-lalr1-grammar 
  (merge-pathnames "pc2.zb" *ZEBU-TEST-DIRECTORY*)
  :output-file (merge-pathnames "pc2.tab"
				*ZEBU-TEST-BINARY-DIRECTORY*)))

(def-tree-attributes Atomic-Wff
    Atomic-Wff--predicate
  )

(def-tree-attributes Role-Argument-Pair
    Role-Argument-Pair--Role
    Role-Argument-Pair--Argument)

(def-tree-attributes Boolean-And
    Boolean-Expr--rand1
    Boolean-Expr--rand2)

(zebu-load-file
 (merge-pathnames "pc1.tab"
		  *ZEBU-TEST-BINARY-DIRECTORY*))


(or (kb-equal (zb:read-parser "walks(agent: John)" 
			      :grammar (zb:find-grammar "pc1"))
	      (zb:read-parser "walks(agent: John)" 
			      :grammar (zb:find-grammar "pc2")))
    (warn "error in grammar pc2: 1"))

(def-tree-attributes Atomic-Wff
    Atomic-Wff--Role-Argument-Pairs
  )

(and (kb-equal (zb:read-parser "walks(agent: John)" :grammar (zb:find-grammar "pc1"))
	       (zb:read-parser "walks(agent: Joe)" :grammar (zb:find-grammar "pc2")))
     (warn "error in grammar pc2: 2"))

(or (kb-equal (zb:read-parser "walks(agent: John) and talks(agent: John)"
			      :grammar (zb:find-grammar "pc1"))
	      (zb:read-parser "walks(agent: John) and talks(agent: John)"
			      :grammar (zb:find-grammar "pc2")))
    (warn "error in grammar pc2: 3"))

(unless (typep (zb:read-parser "walks(agent: John) and talks(agent: John) and Q"
			       :grammar (zb:find-grammar "pc2"))
	       'BOOLEAN-AND)
  (warn "error in grammar pc2: 4"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            recompile NLL grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(and DEFSYS (not (or MCL cmu)))
(let ((nll-sys (probe-file (merge-pathnames
                            "NLL-sys.l"
                            (make-pathname :directory
                                           (append (butlast (pathname-directory
                                                             *ZEBU-directory*))
                                                   (list "nll")))))))
  (when nll-sys
    (load nll-sys)
    (let (zebu:*warn-conflicts* (zebu:*allow-conflicts* t))
      (compile-module "nll-grammar"))
    (load-system 'NLL-test) )
  
  (load-module "test-nll-syntax-1")
  (load-module "test-nll-syntax-2")
  (load-module "nll-simplify-test-1")
  (load-module "nll-simplify-test-2")
  (load-module "drt-to-lgq-test"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 avm grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*))
  (zb:zebu-compile-file "avm.zb"
			:output-file (merge-pathnames
				      "avm.tab" *ZEBU-TEST-BINARY-DIRECTORY*)
			:compile-domain *ZEBU-compile-domain*)
  
  (load (merge-pathnames 
	 (make-pathname :name "avm-p"
			:type (car *load-source-pathname-types*)))))

(zb:zebu-load-file 
 (merge-pathnames "avm.tab" *ZEBU-TEST-BINARY-DIRECTORY*))

(unless (and
	 (equalp (zb:read-parser "[(s1 v1) (s2 v2)]" :grammar (find-grammar "avm"))
		 '#S(FEAT-TERM
		     -TYPE NIL
		     -SLOTS (#S(LABEL-VALUE-PAIR -LABEL S1 -VALUE V1)
			       #S(LABEL-VALUE-PAIR -LABEL S2 -VALUE V2))))
	 (equalp (zb:read-parser "[(s1 v1) (s2 %1= v2) (s3 %1)]"
				 :grammar (find-grammar "avm"))
		 '#S(FEAT-TERM
		     -TYPE NIL
		     -SLOTS (#S(LABEL-VALUE-PAIR -LABEL S1 -VALUE V1)
			       #S(LABEL-VALUE-PAIR
				  -LABEL S2
				  -VALUE #S(TAGGED-TERM
					    -TERM V2
					    -TAG #S(GENERAL-VAR -NAME 1)))
			       #S(LABEL-VALUE-PAIR
				  -LABEL S3
				  -VALUE #S(GENERAL-VAR -NAME 1)))))
	 (equalp (zb:read-parser "type: foo [(s1 v1) (s2 %1= v2) (s3 %1)]"
				 :grammar (find-grammar "avm"))
		 '#S(FEAT-TERM -TYPE FOO
             -SLOTS (#S(LABEL-VALUE-PAIR -LABEL S1 -VALUE V1)
                     #S(LABEL-VALUE-PAIR -LABEL S2
                        -VALUE #S(TAGGED-TERM -TERM V2
                                  -TAG #S(GENERAL-VAR -NAME 1)))
                     #S(LABEL-VALUE-PAIR -LABEL S3
                        -VALUE #S(GENERAL-VAR -NAME 1)))))
	 (equalp (zb:read-parser "%0 = type: foo [(s1 %0) (s2 %1= v2) (s3 %1)]"
				 :grammar (find-grammar "avm"))
		 '#S(TAGGED-TERM
		     -TERM #S(FEAT-TERM
			      -TYPE FOO
			      -SLOTS (#S(LABEL-VALUE-PAIR
					 -LABEL S1
					 -VALUE #S(GENERAL-VAR -NAME 0))
					#S(LABEL-VALUE-PAIR
					   -LABEL S2
					   -VALUE #S(TAGGED-TERM
						     -TERM V2
						     -TAG #S(GENERAL-VAR -NAME 1)))
					#S(LABEL-VALUE-PAIR
					   -LABEL S3
					   -VALUE #S(GENERAL-VAR -NAME 1))))
		     -TAG #S(GENERAL-VAR -NAME 0))))
  (warn "avm grammar did not compile correctly"))


(let ((*load-verbose* t))
  (zb:zebu-compile-file 
   (merge-pathnames "avm1.zb" *ZEBU-TEST-DIRECTORY*)
   :output-file (merge-pathnames "avm1.tab" *ZEBU-TEST-BINARY-DIRECTORY*)
   :compile-domain *ZEBU-compile-domain*))

;;(zebu::print-actions "avm1")
(zb:zebu-load-file 
 (merge-pathnames "avm1.tab" *ZEBU-TEST-BINARY-DIRECTORY*))
(zb:read-parser "[(s1 v1) (s2 v2)]" :grammar (find-grammar "avm1"))
(zb:read-parser "foo: [(s1 v1) (s2 %1= v2) (s3 %1)]"
		:grammar (find-grammar "avm1"))
(zb:read-parser "foo: [(s1 v1) (s2 %1= v2) (s3 %1)]"
		:grammar (find-grammar "avm1"))
(zb:read-parser "foo: []"
		:grammar (find-grammar "avm1"))
(zb:read-parser " []"
		:grammar (find-grammar "avm1"))
(zb:read-parser " [( s1 \"foo\" )]"
		:grammar (find-grammar "avm1"))
(zb:read-parser " [( s1 \"foo\\\"bar\" )]"
		:grammar (find-grammar "avm1"))
(zb:read-parser "foo : [(s1 [(s1 v1)]) (s2 %1= v2) (s3 %1)]"
		:grammar (find-grammar "avm1"))

(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*))
  (zb:file-parser "sample-avm1" :grammar (find-grammar "avm1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 fs-grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(zebu-compile-file (merge-pathnames
                      (make-pathname :name "fsg" :type "zb") *ZEBU-test-directory*)
                     :output-file
                     (merge-pathnames
                      (make-pathname :name "fsg" :type "tab")
                      *ZEBU-test-binary-directory*)
		     :compile-domain *ZEBU-compile-domain*)
(zebu-load-file 
 (merge-pathnames "fsg.tab" *ZEBU-TEST-BINARY-DIRECTORY*))

(read-parser " (:type ATOMIC-WFF) [(PRED walk) (AGENT peter)] "
	     :grammar (find-grammar "tfs-g2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 tdl grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(zebu-compile-file (merge-pathnames
                      (make-pathname :name "hh-tdl" :type "zb") *ZEBU-test-directory*)
                     :output-file
                     (merge-pathnames
                      (make-pathname :name "hh-tdl" :type "tab")
                      *ZEBU-test-binary-directory*)
		     :compile-domain *ZEBU-compile-domain*)
(zebu-load-file 
 (merge-pathnames "hh-tdl.tab" *ZEBU-TEST-BINARY-DIRECTORY*))
(file-parser (merge-pathnames
	      (make-pathname :name "hh-test" :type "tdl") *ZEBU-test-directory*)
	     :grammar (find-grammar "hh-tdl"))

(read-parser
 "index := *avm* &
	 [ PERSON  person,
	   NUMBER  number,
	   GENDER  gender]."
 :grammar (find-grammar "hh-tdl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Regular Expression Compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*load-verbose* t))
  (load (merge-pathnames "regextst.lisp" *ZEBU-TEST-DIRECTORY*))

  (zb:zebu-compile-file 
   (merge-pathnames "pb.zb" *ZEBU-TEST-DIRECTORY*)
   :output-file (merge-pathnames
		 "pb.tab"
		 *ZEBU-TEST-BINARY-DIRECTORY*)
   :compile-domain *ZEBU-compile-domain*))

(zb:zebu-load-file
 (merge-pathnames "pb.tab" *ZEBU-TEST-BINARY-DIRECTORY*))

(unless (equal (zb:read-parser "FOO : bar."
			       :grammar (find-grammar "pb"))
	       '("FOO" BAR))
  (warn "Didn't parse pb grammar expression."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Phone-and-E-Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*)
      (*load-verbose* t))
  (zb:zebu-compile-file "lieber.zb"
			:output-file (merge-pathnames
				      "lieber.tab"
				      *ZEBU-TEST-BINARY-DIRECTORY*)
			:compile-domain *ZEBU-compile-domain*))

(defclass e-mail-address ()
  ((person :initarg :person :accessor person) (host :initarg :host :accessor host)))

(defclass host () ((domain-path :initarg :domain-path :accessor domain-path)))

(defclass phone-number ()
  ((area-code :initarg :area-code :accessor area-code) (exchange :initarg :exchange :accessor exchange)
   (extension :initarg :extension :accessor extension)))

(zb:zebu-load-file
 (merge-pathnames "lieber.tab" *ZEBU-TEST-BINARY-DIRECTORY*))

(find-grammar "Phone-and-E-Mail")


;; This doesn't work...
(read-parser
 "My name is Henry, my address is lieber@media.mit.edu and you can call me at (617) 253-0315" 
 :grammar (find-grammar "Phone-and-E-Mail"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   BibTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+HP300
(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*)
      (*load-verbose* t))
  (zb:zebu-compile-file "bibtex.zb"
			:output-file (merge-pathnames
				      "bibtex.tab" *ZEBU-TEST-BINARY-DIRECTORY*)
			:compile-domain *ZEBU-compile-domain*))

#+HP300
(let ((*default-pathname-defaults* *ZEBU-TEST-BINARY-DIRECTORY*))
  (zb:zebu-load-file "bibtex.tab"))

#+HP300
(read-parser "@TechReport{allen81a,
key\"allen81a\",
author \"ALLEN, J.F.\",
title \"Maintaining Knowledge About Temporal Intervals, TR 86\",
institution \"University of Rochester, Department of Computer Science\",
year \"1981\"}" :grammar (find-grammar "bibtex"))
#+HP300
(progn
  (file-parser "~/notes/lit/bib/time.bib" :grammar (find-grammar "bibtex")
               :print-parse-errors t :verbose nil)
  
  (file-parser "~/notes/lit/bib/functional-lang.bib" :grammar (find-grammar "bibtex")
               :print-parse-errors t :verbose nil)
  
  
  (file-parser "~/notes/lit/bib/cs.bib" :grammar (find-grammar "bibtex")
               :print-parse-errors t :verbose nil)
  
  (file-parser "~/notes/lit/bib/planning.bib" :grammar (find-grammar "bibtex")
               :print-parse-errors t :verbose nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              zebra-grammar.zb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+HP300
(defvar *ZEBRA-DIRECTORY* 
  (let ((d (pathname-directory *ZEBU-TEST-DIRECTORY*)))
    (make-pathname :directory (append (subseq d 0 (- (length d) 2))
				      (list "zebra" "zebra-release")))))
#+HP700 
(defvar *ZEBRA-DIRECTORY* 
  (let ((d (pathname-directory *ZEBU-TEST-DIRECTORY*)))
    (make-pathname :directory (list "zebra" "zebra-release"))))



#+(OR :HP300 :HP700)
(when (and (boundp '*ZEBRA-DIRECTORY*) (probe-file *ZEBRA-DIRECTORY*))
  (let ((*default-pathname-defaults* *ZEBRA-DIRECTORY*)
        (*load-verbose* t))
    (load "ZEBRA-system"))
  )

#+(OR :HP300 :HP700)
(progn
(compile-system "ZEBRA")
(load-system "ZEBRA")

(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*)
      (*load-verbose* t))
  (zb:zebu-compile-file "zebra-grammar.zb"
			:output-file (merge-pathnames
				      "zebra-grammar.tab"
				      *ZEBU-TEST-BINARY-DIRECTORY*)
			:compile-domain *ZEBU-compile-domain*))

(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*)
      (*load-verbose* t))
  (zb:zebu-load-file (merge-pathnames "zebra-grammar.tab"
				      *ZEBU-TEST-BINARY-DIRECTORY*)))

(zb:read-parser "Rule string2terminal :=
                        #1 stringp(#1) --> terminal:[(-string #1)];"
		:grammar (zb:find-grammar "zebra-grammar"))

(defun zebra-read-string (s)
  (zb:read-parser s :grammar (zb:find-grammar "zebra-grammar")))
  
(zebra-read-string "Rule string2terminal :=
                        #1 stringp(#1) --> terminal:[(-string #1)];")
(zebra-read-string "Rule R1 := bar:[(-slot {...})] --> baz:[(-slot {a,b})]; ")
(zebra-read-string "rule t1 := a:[] --> test:[];")
(zebra-read-string "rule t1 := a --> test:[];")
(zebra-read-string "rule t1 := a --> [test];")
(zebra-read-string "rule t1 := a --> b;")

(setq zebu:*current-grammar* (zb:find-grammar "zebra-grammar"))
;;(zebu::print-collection nil)
(zebu::print-productions)

(let ((zb:*preserve-case* t))
  (zebra-read-string "Rule t1:=a --> test;"))

(let ((zb:*preserve-case* t)
      (s " Rule t1:=a --> test;"))
  (string= s (format nil "~a" (zebra-read-string s))))

;;(zb:file-parser (merge-pathnames "zebra-parser.za" *ZEBU-TEST-DIRECTORY*))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+(and LUCID HP300)
(let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*))
  (zb:zebu-load-file
   (zb:zebu-compile-file "time.zb"
			 :output-file (merge-pathnames
				       "time.tab" *ZEBU-TEST-BINARY-DIRECTORY*)
			 :compile-domain *ZEBU-compile-domain*))
  
  )
#+(and LUCID HP300)
(progn
  (zebu::print-actions "time")
  (zebu::print-productions))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Kleene+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((zebu:*allow-conflicts* t)
      (*generate-domain* t))
  (compile-slr-grammar (merge-pathnames "ex5.zb" *ZEBU-TEST-DIRECTORY*)
   :output-file (merge-pathnames "ex5.tab" *ZEBU-TEST-BINARY-DIRECTORY*))
  )
(setq zebu:*current-grammar*
      (zebu-load-file (merge-pathnames "ex5.tab" *ZEBU-TEST-BINARY-DIRECTORY*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               circular print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((zebu:*allow-conflicts* t)
      (*generate-domain* t))
  (compile-slr-grammar (merge-pathnames "ex7.zb" *ZEBU-TEST-DIRECTORY*)
   :output-file (merge-pathnames "ex7.tab" *ZEBU-TEST-BINARY-DIRECTORY*))
  )
(setq zebu:*current-grammar*
      (zebu-load-file (merge-pathnames "ex7.tab" *ZEBU-TEST-BINARY-DIRECTORY*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Kleene +
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((*generate-domain* t))
  (compile-slr-grammar (merge-pathnames "ex8.zb" *ZEBU-TEST-DIRECTORY*)
   :output-file (merge-pathnames "ex8.tab" *ZEBU-TEST-BINARY-DIRECTORY*))
  )
(setq zebu:*current-grammar*
      (zebu-load-file (merge-pathnames "ex8.tab" *ZEBU-TEST-BINARY-DIRECTORY*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   bug-exp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compile-load-test-file (file)
  (let ((*default-pathname-defaults* *ZEBU-TEST-DIRECTORY*))
     (zb:zebu-load-file
      (zb:zebu-compile-file (format nil "~a.zb" file)
			    :output-file (merge-pathnames
					   (format nil "~a.tab" file)
					   *ZEBU-TEST-BINARY-DIRECTORY*)
			    :compile-domain *ZEBU-compile-domain*))
     ))

(let ()
  (compile-load-test-file "bug-exp")
  (compile-load-test-file "bug-exp1")
  (compile-load-test-file "bug-exp2")
  (compile-load-test-file "bug-exp3")
  )

(equal (read-parser "STRING (30)"
		    :grammar (zb:find-grammar "bug-exp"))
       (read-parser "STRING (30)"
		    :grammar (zb:find-grammar "bug-exp1")))

(equal (read-parser "STRING (30)"
		    :grammar (zb:find-grammar "bug-exp2"))
       (read-parser "STRING (30)"
		    :grammar (zb:find-grammar "bug-exp3")))

(equal (let ((*preserve-case* t))
	 (read-parser "fooBar"
		      :grammar (zb:find-grammar "bug-exp3")))
       "fooBar")

;;(debug-parser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Monitoring the Zebu compiler (in Lucid CL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in fresh CL:
#||
(set-working-directory *ZEBU-directory*)
(load "ZEBU-sys.lisp")
(compile-system 'Zebu-compiler)
(with-monitored-definitions (load-system 'Zebu-compiler))
(set-working-directory "../nll/")
(load "NLL-sys.lisp")
(start-monitoring)
(time (compile-module "nll-grammar"))
(summarize-monitors :number-of-calls t)

(start-monitoring)
(time (compile-module "ex1"))
(SUMMARIZE-MONITORS)

(reset-monitors)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            End of exercise.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
