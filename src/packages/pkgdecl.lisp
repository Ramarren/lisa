;;;
;;; File: pkgdecl.lisp
;;; Description: Package declarations for LISA.
;;;
;;; $Id: pkgdecl.lisp,v 1.5 2001/02/03 21:55:22 youngde Exp $

(in-package :cl-user)

(defpackage :lisa
  (:use :common-lisp)
  #+LispWorks
  (:import-from :user pwd cd exit)
  (:shadow :assert))

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(when (and (not (find-package 'clos))
           (find-package 'pcl))
  (rename-package (find-package 'pcl) 'pcl
                  `(clos ,@(package-nicknames 'pcl))))
