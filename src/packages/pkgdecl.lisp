;;;
;;; File: pkgdecl.lisp
;;; Description: Package declarations for LISA.
;;;
;;; $Id: pkgdecl.lisp,v 1.6 2001/03/13 18:54:58 youngde Exp $

(in-package :cl-user)

(defpackage :lisa
  (:use :common-lisp)
  (:shadow :assert)
  (:export "DEFRULE" "DEFTEMPLATE" "ASSERT" "DEFIMPORT" "FACTS" "RULES"
           "AGENDA" "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY" "WATCH" "UNWATCH"
           "WATCHING" "HALT"))

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(when (and (not (find-package 'clos))
           (find-package 'pcl))
  (rename-package (find-package 'pcl) 'pcl
                  `(clos ,@(package-nicknames 'pcl))))
