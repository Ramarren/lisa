;;;
;;; File: defsys.lisp
;;; Description: System definition file for LISA project.
;;;
;;; $Id: defsys.lisp,v 1.3 2000/10/12 20:37:12 youngde Exp $

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

(let ((dirlist '("packages" "engine" "utils")))
  (unless (probe-file *lisa-binary-pathname*)
    (mkdir *lisa-binary-pathname*))
  (dolist (dir dirlist)
    (let ((path (make-pathname
                 :directory (append (pathname-directory
                                     *lisa-binary-pathname*)
                                    `(,dir)))))
      (unless (probe-file path)
        (mkdir path)))))

(mk:define-language :zebu
    :compiler #'(lambda (file &key output-file error-file)
                  (declare (ignore error-file))
                  (apply #'zb:zebu-compile-file file `(:output-file ,output-file)))
    :loader #'(lambda (&rest args)
                (apply #'zb:zebu-load-file args))
    :source-extension "zb"
    :binary-extension "tab")

(mk:defsystem "lisa"
    :source-pathname *lisa-source-pathname*
    :binary-pathname *lisa-binary-pathname*
    :source-extension "lisp"
    :components ((:module "packages"
                          :source-pathname "packages"
                          :binary-pathname "packages"
                          :components ((:file "pkgdecl")))
                 (:module "grammar"
                          :source-pathname "engine"
                          :binary-pathname "engine"
                          :language :zebu
                          :source-extension "zb"
                          :binary-extension "tab"
                          :components ((:file "lisa"))
                          :depends-on (packages))
                 (:module "engine"
                          :source-pathname "engine"
                          :binary-pathname "engine"
                          :components ((:file "language"))
                          :depends-on (packages)))
    :depends-on (zebu-compiler))
