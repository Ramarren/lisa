;;;
;;; File: pkgdecl.lisp
;;; Description: Package declarations for LISA.
;;;
;;; $Id: pkgdecl.lisp,v 1.3 2000/11/16 02:47:22 youngde Exp $

(in-package :cl-user)

(defpackage :lisa
  (:use :common-lisp)
  (:shadow "ASSERT"))
