; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         ZEBU-init.lisp
; Description:  Loading Zebu and the Compiler
; Author:       Joachim H. Laubsch
; Created:      19-May-92
; Modified:     Thu Jan  7 11:15:55 1999 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/ZEBU-init.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: ZEBU-init.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to use or compile Zebu, you must load this file first

;; to load the Zebu Compiler, it has to be compiled
;;  To compile the Zebu Compiler load COMPILE-ZEBU.l

;; zb:zebu to               "Load the Zebu Parser"
;; zb:zebu-compiler to      "Load the Zebu Compiler"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-(or CLISP MCL) (in-package "USER")
#+(or CLISP MCL) (in-package "CL-USER")

#-(or CLISP MCL CMU)
(progn
  (unless (find-package "CL-USER")
    (defpackage "USER" (:nicknames "CL-USER")))
  (in-package "CL-USER"))

(provide "ZEBU-init")

;; edit the following form for your Lisp, and the directory where you keep Zebu
(defparameter *ZEBU-directory*
  (make-pathname 
   :directory
   (pathname-directory
    #+(or CLISP CMU)  *load-truename*
    #-ALLEGRO #+MCL (truename *loading-file-source-file*)
    #-ALLEGRO #-MCL #-CMU *load-pathname*
    #+ALLEGRO (merge-pathnames *load-pathname*
                               *default-pathname-defaults*)))
  )

;----------------------------------------------------------------------------;
; *ZEBU-binary-directory*
;------------------------
; directory for compiled grammars and lisp files
; 
(defparameter *ZEBU-binary-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-directory*)
				    (list "binary"))))

#+(or MCL Allegro CMU)
(defvar *load-binary-pathname-types* '("fasl"))
#+CLISP
(defvar *load-binary-pathname-types* '("fas"))
#+(and :SUN :LUCID)
(setq *load-binary-pathname-types* '("sbin"))

#+(or MCL Allegro CLISP)
(setq *load-source-pathname-types* '("lisp" NIL))
#+(or (and WINDOWS ACL3.0) :HARLEQUIN-PC-LISP)
(defvar *load-source-pathname-types* '("lsp" NIL))
#+CMU
(defvar *load-source-pathname-types* '("lisp" NIL))
#+(or (and WINDOWS ACL3.0) :HARLEQUIN-PC-LISP)
(defvar *load-binary-pathname-types* '("fsl"))

(unless (probe-file *ZEBU-binary-directory*)
  #+LUCID
  (shell (format nil "mkdir ~a" (namestring *ZEBU-binary-directory*)))
  #+ALLEGRO
  (let ((dir (format nil "~abinary"
		     (namestring *ZEBU-directory*))))
    (unless (zerop (run-shell-command
		    (format nil "mkdir ~a" dir)))
      (error "Could not create directory ~s" dir)))
  #+CMU
  (unix:unix-mkdir
   (format nil "~A/binary" (directory-namestring *ZEBU-directory*))
   (logior unix:readown unix:writeown
           unix:execown unix:readgrp unix:execgrp
           unix:readoth unix:execoth))
  #+MCL
  (create-file *ZEBU-binary-directory* :if-exists nil)
  #+(and WINDOWS ACL3.0)
  (create-directory *ZEBU-binary-directory*)
)

;----------------------------------------------------------------------------;
; ZEBU package aka ZB
;-------------
; 
(let ((*default-pathname-defaults* *ZEBU-directory*))
  (load (merge-pathnames 
	 (make-pathname :name "zebu-package"
			:type (car *load-source-pathname-types*)))))

(shadowing-import '(zb::zebu zb::zebu-compiler) (find-package "CL-USER"))
(export '(zb::zebu zb::zebu-compiler) (find-package "ZB"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter zb:*zebu-version*
  (let ((file (make-pathname
		 :name "Version"
		 :type nil
		 :directory (pathname-directory *ZEBU-directory*))))
      (when (probe-file file)
	(with-open-file (s file :direction :input)
	  (read-line s)))))


;----------------------------------------------------------------------------;
; zb:zebu
;--------
; load the zebu parser runtime system
; 
(defun zb:zebu (&key (compiled t))
  "Load the Zebu parser"
  (let ((defaults 
            (merge-pathnames
             (make-pathname :type (first (if compiled
                                             *load-binary-pathname-types*
                                           *load-source-pathname-types*)))
             (if compiled
                 *ZEBU-binary-directory*
               *ZEBU-directory*))))
    (dolist (name '("zebu-aux"
		    "zmg-dom"
		    "zebu-mg-hierarchy"
		    "zebu-loader"
		    "zebu-driver"
		    "zebu-actions"))
      (let ((file (merge-pathnames (make-pathname :name name) defaults))) 
        (if (probe-file file)
            (require name file)
          (require name
                   (merge-pathnames (make-pathname
                                     :name name
                                     :type (car *load-source-pathname-types*))
                                    *ZEBU-directory*)))))
    (format t "~%;;; Zebu (Version ~a) loaded!~%" zb:*zebu-version*)
    (values)))

;----------------------------------------------------------------------------;
; zb:zebu-compiler
;-----------------
; load the Zebu Compiler
; 
(defun zb:zebu-compiler (&key (compiled t) (verbose t))
  "Load the Zebu Compiler"
  (zb::zebu :compiled compiled)
  (push ':ZEBU *features*)
  (let ((defaults 
            (merge-pathnames
             (make-pathname
              :type (car (if compiled 
                             *load-binary-pathname-types*
                           *load-source-pathname-types*)))
             (if compiled
                 *ZEBU-binary-directory*
               *ZEBU-directory*)))
        (*load-verbose* verbose))
    (dolist (name '("zebu-kb-domain" "zebu-regex"
                    "zebu-oset" "zebu-g-symbol" "zebu-loadgram"
                    "zebu-generator" "zebu-lr0-sets"
                    "zebu-empty-st" "zebu-first"
                    "zebu-follow" "zebu-tables"
                    "zebu-slr" "zebu-closure"
                    "zebu-lalr1" "zebu-dump" "zebu-compile"
                    "zebu-printers"))
      (unless (member name *modules* :test #'string=)
        (load (merge-pathnames (make-pathname :name name) defaults)))) 
    (ZB:zebu-compile-file
     (merge-pathnames (make-pathname :name "zebu-mg" :type "zb")
                      *ZEBU-directory*)
     :output-file (merge-pathnames (make-pathname :name "zebu-mg")
                                   *ZEBU-binary-directory*)
     :verbose verbose
     :compile-domain compiled)
    (zb::zebu-load-file (merge-pathnames
                         (make-pathname :name "zebu-mg" :type "tab")
                         *ZEBU-binary-directory*))
    (format t "~%;;; Zebu Compiler (Version ~a) loaded!~%" zb:*zebu-version*)
    (values)))

(defun zb:zebu-rr (&key (compiled t))
  "Load the rewrite-rule module"
  (zb::zebu)
  (let ((path (merge-pathnames
	       (make-pathname
		:type (car (if compiled 
			       *load-binary-pathname-types*
			     *load-source-pathname-types*)))
	       (if compiled
		   *ZEBU-binary-directory*
		 *ZEBU-directory*))))
    (dolist (name '("zebu-kb-domain" "zebu-tree-attributes"
		    "zebra-debug"))
      (unless (member name *modules* :test #'string=)
	(load (merge-pathnames (make-pathname :name name) path))))
    (values)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You may want to omit this and rather import only a subset of the 
;; symbols or use package "ZEBU" in another package than the CL-USER
;; package.

(use-package (find-package "ZEBU"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               A few Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||
(load "ZEBU-init.lisp")
;; load the compiler
#+(and :MSWINDOWS :ALLEGRO) 
(zb:zebu-compiler  :compiled  nil)

#-(and :MSWINDOWS :ALLEGRO)
(zb:zebu-compiler)

(setq *ZEBU-test-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-directory*)
				    (list "test"))))
(setq *ZEBU-test-binary-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-test-directory*)
				    (list "binary"))))

(unless (probe-file *ZEBU-test-binary-directory*)
  #+LUCID
  (shell (format nil "mkdir ~a" (namestring *ZEBU-test-binary-directory*)))
  #+MCL
  (create-file *ZEBU-test-binary-directory* :if-exists nil)
  #+(and WINDOWS ACL3.0)
  (create-directory *ZEBU-test-binary-directory*)  
  )

(zebu-compile-file (merge-pathnames
		    (make-pathname :name "ex1" :type "zb") *ZEBU-test-directory*)
		   :output-file
		   (merge-pathnames
		    (make-pathname :name "ex1" :type "tab")
		    *ZEBU-test-binary-directory*))

(zb:zebu-load-file (merge-pathnames
		    (make-pathname :name "ex1" :type "tab")
                    *ZEBU-test-binary-directory*))

(setq zebu:*current-grammar* (zb:find-grammar "ex1"))
(list-parser '(1 "+" 1))
(equal (read-parser "1 + 1") (list-parser '(1 "+" 1)))
(read-parser "1.0 * 1")
(read-parser "1.0 * 1/33")
(read-parser "1.0 * a1")
(read-parser "1.0 * .3")
(read-parser "1.0 * 12.3")

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             End of ZEBU-init.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
