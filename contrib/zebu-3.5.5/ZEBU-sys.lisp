; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         ZEBU-sys.lisp
; Description:  Definition of ZEBU, the cousin of Yacc (Runtime & Compiler)
; Author:       Joachim H. Laubsch
; Created:      11-Oct-90
; Modified:     Fri Oct  3 10:53:02 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/ZEBU-sys.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: ZEBU-sys.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "CL-USER")

(require "P-defsys")
(use-package "DS" (find-package "CL-USER"))

;; the binaries will be defined in a subdirectory camed "binary":
(setq ds:*relative-binary-namestring* "binary")
#+:KCL (setf (car ds:*suffixes*) "l")



;; edit the following line to the pathname of this file if 
;; *LOAD-PATHNAME* is undefined in the CL used
#+ALLEGRO 
(setf *load-pathname* (merge-pathnames *load-pathname*
                                       *default-pathname-defaults*))
#+cmu
(setf *load-pathname* *load-truename*)
 
#+LUCID
(proclaim '(special *ZEBU-directory* *ZEBU-binary-directory*
	    *ZEBU-test-directory* *ZEBU-test-binary-directory*))
#-LUCID
(declaim (special *ZEBU-directory* *ZEBU-binary-directory*
                  *ZEBU-test-directory* *ZEBU-test-binary-directory*))

(defparameter *ZEBU-directory*
  (make-pathname :directory (pathname-directory *load-pathname*))
  )

(setf *ZEBU-binary-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-directory*)
				    (list "binary"))))

(setf *ZEBU-test-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-directory*)
				    (list "test"))))

(setf *ZEBU-test-binary-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-test-directory*)
				    (list "binary"))))

;; create binary directory if necessary
(progn
  #+LUCID
  (or (probe-file *ZEBU-test-binary-directory*)
      (shell (format nil "mkdir ~a" (namestring *ZEBU-test-binary-directory*))))
  #+MCL
  (create-file *ZEBU-test-binary-directory* :if-exists nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Define the ZEBU package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (merge-pathnames "zebu-package.lisp" *ZEBU-directory*))

(defpackage "ZEBU-TEST"
    (:use "ZEBU" #+LUCID "LISP" #-LUCID "COMMON-LISP") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation: Production mode

#+LUCID(proclaim '(optimize (speed 3) (safety 1) (compilation-speed 0)))
#-LUCID(declaim (optimize (speed 3) (safety 1) (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Systems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ds:defsystem "ZEBU-KERNEL"
    (:default-pathname #.*ZEBU-directory*
     :default-package  "ZEBU"
     :documentation    "Functions needed in the ZEBU run-time and compile-time environment"
     )
  "zebu-package"
  ("Version"              :type :text  :suffixes "")
  ("zebu-aux"             :load-always "zebu-package"
			  :recompile-on "Version")
  ("zebu-mg-hierarchy"    :load-always "zebu-aux")
  )
  
(ds:load-module "zebu-package" :system "ZEBU-KERNEL")

(ds:defsystem "ZEBU"
    (:default-pathname    #.*ZEBU-directory*
     :default-package     "ZEBU"
     :load-before-compile "ZEBU-KERNEL"
     :needed-systems      "ZEBU-KERNEL"
     :documentation       "Run time system for LALR(1) parser"
     )
  ("zebu-loader"          :load-always ("zebu-package" "zebu-aux"))
  ("zebu-driver"          :load-always ("zebu-loader" "zebu-package"))
  ("zebu-actions" :load-always ("zebu-loader"))
  )

(ds:defsystem "ZEBU-COMPILER"
    (:default-pathname    #.*ZEBU-directory*
     :default-package     "ZEBU"	
     :needed-systems      "ZEBU"
     :load-before-compile "ZEBU-KERNEL"
     :documentation       "Compile time system for LALR(1) parser: Converts a grammar to a parse table"
     )
  "zebu-regex"
  "zebu-oset"
  ("zebu-g-symbol"  :load-always "zebu-oset")
  ("zebu-loadgram"  :load-always ("zebu-g-symbol" "zebu-oset"
				  ("ZEBU" "zebu-loader")))
  ("zebu-generator" :load-always ("zebu-kb-domain" "zebu-loadgram"))
  ("zebu-lr0-sets"  :load-always ("zebu-g-symbol" "zebu-loadgram"))
  ("zebu-empty-st"  :load-always ("zebu-loadgram"))
  ("zebu-first"     :load-always ("zebu-loadgram" "zebu-oset")
		    :recompile-on "zebu-oset")
  ("zebu-follow"    :load-always ("zebu-loadgram" "zebu-first"))
  ("zebu-tables"    :load-always (("ZEBU" "zebu-loader")
				  "zebu-g-symbol" "zebu-loadgram"
				  "zebu-lr0-sets"))
  ("zebu-printers"  :load-always ("zebu-loadgram" "zebu-lr0-sets" "zebu-tables"))
  ("zebu-slr"       :load-always (("ZEBU" "zebu-loader")))
  ("zebu-closure"   :load-always ("zebu-oset" "zebu-g-symbol" "zebu-first"))
  ("zebu-lalr1"     :load-always (("ZEBU" "zebu-loader")
				  "zebu-oset" "zebu-lr0-sets" "zebu-follow"))
  ("zebu-dump"      :load-always ("zebu-loadgram" "zebu-slr" "zebu-lalr1"))
  ("zebu-compile"   :load-before-compile "zebu-dump"
		    :load-always (("ZEBU-KERNEL" t)))
  ("zebu-mg"        :load-always ("zebu-compile" "zebu-dump"
				  "zebu-empty-st" "zebu-closure"
				  "zebu-tables" "zebu-generator"
				  ("ZEBU-KERNEL" t))
		    :compiler    ZB:zebu-compile-file
		    :loader      ZB:zebu-load-file
		    :suffixes    ("zb" . "tab"))
  ("zmg-dom"        :recompile-on "zebu-mg")
  "zebu-kb-domain" 
  )

(ds:defsystem "ZEBU-RR"
       (:default-pathname    #.*ZEBU-directory*
	:needed-systems      "ZEBU"
	:load-before-compile "ZEBU-KERNEL"
	:documentation       "Base routines for rewriting abstract syntax trees"
	)
  "zebu-kb-domain"
  ("zebu-tree-attributes"  :load-always  "zebu-kb-domain")
  ("zebra-debug"           :load-always  ("zebu-kb-domain"
					  "zebu-tree-attributes"))
  )

(ds:defsystem zb::ZEBU-Test
    (:default-pathname  #.*ZEBU-test-directory*
     :needed-systems    ("ZEBU" "ZEBU-COMPILER")
     :suffixes          ("zb" . "tab")
     :compiler          zb:zebu-compile-file
     :loader            zb:zebu-load-file
     )
  "ex1"
  ("ex1-dom" :recompile-on "ex1" :type :lisp)
  "arith"				; this uses the meta-grammar
  ("ar-dom" :recompile-on "arith" :type :lisp)
  "ex2"
  ("ex2-dom" :recompile-on "ex2" :type :lisp)
  "ex3"
  "ex4.40"
  "ex4.41"
  "ex4.42"
  "mini-la"				; this uses the meta-grammar w Kleene
  ("minl-dom" :recompile-on "mini-la" :compile-only t :type :lisp)
  "useless"
  "simple"
  "lr4-21"
  ("lr4-dom" :recompile-on "lr4-21" :compile-only t :type :lisp)
  "ex6_2"
  ("ex6-dom" :recompile-on "ex6_2" :compile-only t :type :lisp)
  "pc"
  ("pc-dom" :recompile-on "pc" :compile-only t :type :lisp)
  "pc1"					; this uses the null-grammar
  ("pc1-dom" :recompile-on "pc1" :compile-only t :type :lisp)
  "pc2"					; this uses the meta-grammar
  ("pc2-dom" :recompile-on "pc2" :compile-only t :type :lisp)
  ;; don't compile the following:
  ("exercise"   :suffixes  "lisp"  :type :lisp-example
		:load-always (("ZEBU-RR" T)))
  ("regextst" :suffixes  "lisp"  :type :lisp-example)
  )
(provide "ZEBU-sys")

#||  Instructions

;; (1) Load THIS file
;; be sure that you have set the environment-variable "ZEBU"
;; to the current directory
(let* ((zebudir (or (environment-variable "ZEBU")
		    (if (environment-variable "HOME")
			(format nil "~a/zebu/" (environment-variable "HOME"))
		      (pwd))))
       (this (merge-pathnames
	      (make-pathname :name "ZEBU-sys" :type "lisp")
	      zebudir)))
  (if (probe-file this)
      (load this)
    (error "~a not found " this)))

;; (2) to compile the ZEBU runtime system
(ds:compile-system "ZEBU"
		;; :recompile T
		:include-components T)

;; (3) to compile the ZEBU-Compiler system
(ds:compile-system "ZEBU-COMPILER"
		;; :recompile T
		)
;; (4) to load the ZEBU runtime system
(ds:load-system "ZEBU")

;; (5) to load the ZEBU-Compiler system
(ds:load-system "ZEBU-COMPILER")

(ds:compile-system "ZEBU-RR")

;; (6) to compile the test-gramamrs
;; You may want to omit this and rather import only a subset of the 
;; symbols or use package "ZEBU" in another package.

(use-package (find-package "ZEBU")
	     (find-package "CL-USER"))

(ds:compile-system "ZEBU-TEST"
		   :recompile t :include-components nil)
(ds:show-system "ZEBU-TEST")

;; (7) to test Zebu
(ds:load-system "ZEBU-TEST")
(ds:load-module "exercise")

;; (8) the rewrite-rule extension
(ds:compile-system "ZEBU-RR")
(ds:load-system "ZEBU-RR")
 
(ds:show-system "ZEBU")
(ds:show-system "ZEBU-KERNEL")
(ds:show-system "ZEBU-COMPILER")

(ds:compile-module "zebu-mg")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1 Grammar
; (:name "ex1" :package "CL-USER")

; (defrule EE
;  := ( EE "+" TT )
;  :build (LIST 'expression EE '+ TT)
;  := TT
;  :build (LIST 'expression TT))

; (defrule  TT 
;   := (TT "*" F)
;   :build (LIST 'term TT '* F)

;   := F
;   :build (LIST 'term F) )

; (defrule F 
;   := ( "(" EE ")" )
;   :build (LIST 'factor "(" EE ")")
;   
;   := IDENTIFIER
;   :build (list 'factor IDENTIFIER)
;   
;   := NUMBER
;   :build (list 'factor NUMBER))

(let ((*load-verbose* t))
  (compile-slr-grammar
   (merge-pathnames "ex1.zb" *ZEBU-test-directory*)
   :output-file "/tmp/ex1.tab"))
(setq zebu:*current-grammar*
      (zebu-load-file "/tmp/ex1.tab"))
(progn (format t "symbols: ") (terpri) (zebu::cruise-symbols-2))
(zebu::print-productions)
(zebu::print-collection nil)
;;(zebu::print-collection t)
(print-actions (zebu::grammar-name zebu:*current-grammar*))
(equal (read-parser "1 + a") (read-parser "1 + A"))
(equal (list-parser '(1 "+" a)) (read-parser "1 + A"))

(list-parser '(1 "+" 1))
(equal (read-parser "1 + 1") (list-parser '(1 "+" 1)))
(equal (read-parser "1 + x * y") (list-parser '(1 "+" x "*" y)))
(equal (read-parser "(1 + x) * y") (list-parser '("(" 1 "+" x ")" "*" y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ex2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (:name "ex2" :package "USER")

; (defrule EE 
; 	:= (TT E-PRIME)
; 	:build (cons TT E-PRIME) )

; (defrule E-PRIME 
; 	:= ("+" TT E-PRIME)
; 	:build list*

; 	:= ())

; (defrule TT 
; 	:= (FF T-PRIME)
; 	:build (cons FF T-PRIME) )

; (defrule T-PRIME 
; 	:= ("*" FF T-PRIME) 
; 	:build list*

; 	:= ())

; (defrule FF
; 	:= ( "(" EE ")") 
; 	:= IDENTIFIER )

(compile-slr-grammar (merge-pathnames "ex2.zb" *ZEBU-test-directory*)
		     :output-file "/tmp/ex2.tab")
(setq zebu:*current-grammar*
      (setq $G1 (zebu-load-file "/tmp/ex2.tab")))
(zebu::cruise-follow-sets)
(zebu::print-productions)
(zebu::cruise-parse-tables)
(print-actions (zebu::grammar-name zebu:*current-grammar*))

(read-parser "ned + jed" :grammar $G1)
(list-parser '(ned "+" jed ) :grammar $G1)
(list-parser '(ned "+" jed ))
(list-parser '(ned "+" "(" jed "*" fred ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ex3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (:name "ex3")

; (defrule a 
; 	:= "b"
; 	:= ())

; (defrule c 
; 	:= "b"
; 	:= ())


; (defrule d 
; 	:= (a c a))

; (defrule e
; 	:= (a "f" a))


(let (zebu:*allow-conflicts*)
  (compile-lalr1-grammar
   (merge-pathnames "ex3.zb" *ZEBU-test-directory*)
   :output-file "/tmp/ex3.tab"))
(setq $G3 (zebu-load-file "/tmp/ex3.tab"))
(print-actions "ex3")

(list-parser '("b") :grammar $G3)
(list-parser '() :grammar $G3)

(let ((zebu:*allow-conflicts* t))
  (compile-slr-grammar
   (merge-pathnames "ex3.zb" *ZEBU-test-directory*)
   :output-file "/tmp/ex3.tab"))

(let ((zebu:*allow-conflicts* t))
  (compile-lalr1-grammar
   (merge-pathnames "ex3.zb" *ZEBU-test-directory*)
   :output-file "/tmp/ex3.tab"))

(setq zebu:*current-grammar* (zebu-load-file "/tmp/ex3.tab"))
(list-parser '( "b" ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  dangelse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (let ((zebu:*allow-conflicts* t) (zebu::*WARN-CONFLICTS* t))
;   (compile-lalr1-grammar
;    (merge-pathnames "dangelse.zb" *ZEBU-test-directory*)
;    :output-file "/tmp/dangelse.tab"))
; equivalent to:
(zebu-compile-file "$zebutest/dangelse.zb"
		   :output-file "/tmp/dangelse.tab")
(zebu-load-file "/tmp/dangelse.tab")

(setq zebu:*current-grammar* (zb:find-grammar "dangelse"))
(print-actions (zebu::grammar-name zebu:*current-grammar*))

(equal (list-parser '("if" f "then" g "else" h))
       (read-parser "if f then g else h"))

(read-parser "if f then g ")

(read-parser "if f then if g then g1 else g2 else h")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  lr4-21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(zebu-compile-file "$zebutest/lr4-21.zb"
		   :output-file "$zebutest/lr4-21.tab")
(zebu-load-file "$zebutest/lr4-21.tab")

(print-actions "lr4-21")
(setq zebu:*current-grammar* (zb:find-grammar "lr4-21"))
(read-parser "foo = 0")
(read-parser "foo = x")
(read-parser "*foo = x")
(read-parser "*0 = x")
(read-parser "**foo = ***x")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Propositional Calculus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((zebu:*allow-conflicts* nil))
  (set-working-directory *ZEBU-test-directory*)
  (compile-lalr1-grammar "pc1.zb")
  (load (merge-pathnames "pc1-hierarchy" *ZEBU-test-directory*))
  (load (merge-pathnames "pc1-printers" *ZEBU-test-directory*))
  (setq zebu:*current-grammar* (zebu-load-file "pc1.tab"))
  )

(read-parser "P")
(read-parser "P and Q")
(read-parser "P and Q and R")
(read-parser "P and Q or R and S")
(read-parser "(P and Q) or R and S")
(read-parser "P and (Q or R) and S")
(read-parser "P(a: 1 b:S)")

(print-actions "pc1")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  lr4-21.zb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(zebu::compile-lalr1-grammar lr4-21.zb")
(setq zebu:*current-grammar* (zebu-load-file "lr4-21.tab"))

(read-parser "x = 3")
(read-parser "x = * * y")

(progn
  (format t "~%lr0 item sets: ~%") 
  (zebu::print-collection t)
  (zebu::CRUISE-FOLLOW-SETS)
  (format t "~%~%lalr(1) tables:~%")
  (zebu::cruise-parse-tables)
  )
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            End of ZEBU-sys.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
