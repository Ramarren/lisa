;;;
;;; File: pkgdecl.lisp
;;; Description: Package declarations for LISA.
;;;
;;; $Id: pkgdecl.lisp,v 1.4 2000/11/16 20:47:13 youngde Exp $

(in-package :cl-user)

(defpackage :lisa
  (:use #:common-lisp)
  (:shadow #:assert))

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(when (and (not (find-package 'clos))
           (find-package 'pcl))
  (rename-package (find-package 'pcl) 'pcl
                  `(clos ,@(package-nicknames 'pcl))))
