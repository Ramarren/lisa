;;; File: defsys.lisp
;;; Description: System definition file for ZEBU, adapted for MK:DEFSYSTEM.
;;;
;;; NB: This file probably requires DEFSYSTEM release 3.2i, as found in the
;;; Common Lisp Open Code Collection (CLOCC). At least, that's the only version
;;; against which I've tested. Currently, the CLOCC project resides at
;;; http://clocc.sourceforge.net.
;;;
;;; Adaptation by David E. Young (de.young@computer.org)
;;;
;;; $Id: defsys.lisp,v 1.2 2000/10/12 20:23:18 youngde Exp $

(in-package "CL-USER")

(defparameter *ZEBU-directory*
  (make-pathname 
   :directory
   (pathname-directory
    #+(or CLISP CMU)  *load-truename*
    #-ALLEGRO #+MCL (truename *loading-file-source-file*)
    #-(or ALLEGRO MCL CMU) *load-pathname*
    #+ALLEGRO (merge-pathnames *load-pathname*
                               *default-pathname-defaults*))))

(defparameter *ZEBU-binary-directory*
  (make-pathname :directory (append (pathname-directory *ZEBU-directory*)
				    (list "binary"))))

#+(or MCL Allegro CLISP)
(setq *load-source-pathname-types* '("lisp" NIL)
      *load-binary-pathname-types* '("fasl"))

#+CMU
(defvar *load-source-pathname-types* '("lisp" NIL))
#+CMU
(defvar *load-binary-pathname-types* '("fasl"))

;; create binary directory if necessary

(unless (probe-file *ZEBU-binary-directory*)
  #+ALLEGRO
  (let ((dir (format nil "~A/binary"
		     (namestring *ZEBU-directory*))))
    (unless (zerop (run-shell-command
		    (format nil "mkdir ~A" dir)))
      (error "Could not create directory ~S" dir)))
  #+CMU
  (unix:unix-mkdir
   (format nil "~A/binary" (directory-namestring *ZEBU-directory*))
   (logior unix:readown unix:writeown
           unix:execown unix:readgrp unix:execgrp
           unix:readoth unix:execoth))
  #+MCL
  (create-file *ZEBU-binary-directory* :if-exists nil))

(load (merge-pathnames "zebu-package.lisp" *ZEBU-directory*))

(defparameter zb:*zebu-version*
  (let ((file (make-pathname
		 :name "Version"
		 :type nil
		 :directory (pathname-directory *ZEBU-directory*))))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (read-line s)))))

(mk:define-language :zebu
    :compiler #'(lambda (file &key output-file error-file)
                  (declare (ignore error-file))
                  (apply #'zb:zebu-compile-file file `(:output-file ,output-file)))
    :loader #'(lambda (&rest args)
                (apply #'zb:zebu-load-file args))
    :source-extension "zb"
    :binary-extension "tab")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Systems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mk:defsystem "zebu"
    :source-pathname *ZEBU-directory*
    :binary-pathname *ZEBU-binary-directory*
    :source-extension "lisp"
    :binary-extension "fasl"
    :components ((:module "zebu-kernel"
                          :source-pathname ""
                          :binary-pathname ""
                          :components ((:file "zebu-aux")
                                       (:file "zebu-mg-hierarchy"
                                              :depends-on (zebu-aux))))
                 (:module "zebu-runtime"
                          :source-pathname ""
                          :binary-pathname ""
                          :components ((:file "zebu-loader")
                                       (:file "zebu-driver"
                                              :depends-on (zebu-loader))
                                       (:file "zebu-actions"
                                              :depends-on (zebu-loader)))
                          :depends-on (zebu-kernel))))

(mk:defsystem "zebu-compiler"
    :source-pathname *ZEBU-directory*
    :binary-pathname *ZEBU-binary-directory*
    :source-extension "lisp"
    :binary-extension "fasl"
    :depends-on (zebu)
    :components ((:module "compiler"
                          :source-pathname ""
                          :binary-pathname ""
                          :components ((:file "zebu-regex")
                                       (:file "zebu-oset")
                                       (:file "zebu-kb-domain")
                                       (:file "zebu-g-symbol"
                                              :depends-on (zebu-oset))
                                       (:file "zebu-loadgram"
                                              :depends-on (zebu-g-symbol
                                                           zebu-oset))
                                       (:file "zebu-generator"
                                              :depends-on (zebu-kb-domain
                                                           zebu-loadgram))
                                       (:file "zebu-lr0-sets"
                                              :depends-on (zebu-g-symbol
                                                           zebu-loadgram))
                                       (:file "zebu-empty-st"
                                              :depends-on (zebu-loadgram))
                                       (:file "zebu-first"
                                              :depends-on (zebu-loadgram
                                                           zebu-oset))
                                       (:file "zebu-follow"
                                              :depends-on (zebu-loadgram
                                                           zebu-oset))
                                       (:file "zebu-tables"
                                              :depends-on (zebu-g-symbol
                                                           zebu-loadgram
                                                           zebu-lr0-sets))
                                       (:file "zebu-printers"
                                              :depends-on (zebu-loadgram
                                                           zebu-lr0-sets
                                                           zebu-tables))
                                       (:file "zebu-slr")
                                       (:file "zebu-closure"
                                              :depends-on (zebu-oset
                                                           zebu-g-symbol
                                                           zebu-first))
                                       (:file "zebu-lalr1"
                                              :depends-on (zebu-oset
                                                           zebu-lr0-sets
                                                           zebu-follow))
                                       (:file "zebu-dump"
                                              :depends-on (zebu-loadgram
                                                           zebu-slr
                                                           zebu-lalr1))
                                       (:file "zebu-compile"
                                              :depends-on (zebu-dump))))
                 (:module "grammar"
                          :source-pathname ""
                          :binary-pathname ""
                          :language :zebu
                          :source-extension "zb"
                          :binary-extension "tab"
                          :components ((:file "zebu-mg"))
                          :depends-on (compiler))
                 (:module "domain"
                          :source-pathname ""
                          :binary-pathname ""
                          :components ((:file "zmg-dom"))
                          :depends-on (grammar))))

(mk:defsystem "zebu-rr"
    :source-pathname *ZEBU-directory*
    :binary-pathname *ZEBU-binary-directory*
    :source-extension "lisp"
    :binary-extension "fasl"
    :depends-on (zebu-compiler zebu)
    :components ((:file "zebu-tree-attributes")))
