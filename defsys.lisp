;;;
;;; File: defsys.lisp
;;; Description: System definition file for LISA project.
;;;
;;; $Id: defsys.lisp,v 1.2 2000/10/10 20:42:28 youngde Exp $

(in-package "USER")

(defvar *lisa-root-pathname*
  (make-pathname :directory
                 (pathname-directory
                  (merge-pathnames *load-truename*
                                   *default-pathname-defaults*))))

(defvar *lisa-source-pathname*
  (make-pathname :directory
                 (append (pathname-directory *lisa-root-pathname*)
                         '("src"))))

(defvar *lisa-binary-pathname*
  (make-pathname :directory
                 (append (pathname-directory *lisa-root-pathname*)
                         #+Allegro '("lib" "acl")
                         #+LispWorks '("lib" "lispworks")
                         #+CMU '("lib" "cmucl")
                         #-(or Allegro LispWorks CMU) (error "Unsupported implementation."))))

(defun mkdir (path)
  #+CMU
  (unix:unix-mkdir
   (directory-namestring path)
   (logior unix:readown unix:writeown
           unix:execown unix:readgrp unix:execgrp
           unix:readoth unix:execoth))
  #+Allegro
  (excl:make-directory path)
  #+Lispworks
  (system:make-directory path))
  
;; Make sure the binary directory structure exists, creating it if
;; necessary...

(when (not (probe-file *lisa-binary-pathname*))
  (mkdir *lisa-binary-pathname*))

(mk:defsystem "lisa"
    :source-pathname *lisa-source-pathname*
    :binary-pathname *lisa-binary-pathname*
    :source-extension "lisp"
    :components ((:module "packages"
                          :source-pathname "packages"
                          :binary-pathname "packages"
                          :components ((:file "pkgdecl")))
                 (:module "engine"
                          :source-pathname "engine"
                          :binary-pathname "engine"
                          :components ((:file "language"))
                          :depends-on (packages))))
