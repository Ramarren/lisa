;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: pkgdecl.lisp
;;; Description: Package declarations for LISA.

;;; $Id: pkgdecl.lisp,v 1.11 2001/04/10 20:50:47 youngde Exp $

(in-package "CL-USER")

(defpackage "LISA"
  (:use "COMMON-LISP")
  (:shadow "ASSERT")
  (:export "DEFRULE" "DEFTEMPLATE" "ASSERT" "DEFIMPORT" "FACTS" "RULES"
           "AGENDA" "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY" "WATCH" "UNWATCH"
           "WATCHING" "HALT"))

(defpackage "LISA.REFLECT"
  (:use "COMMON-LISP")
  (:nicknames "REFLECT")
  #+(or Allegro CMUCL)
  (:import-from "CLOS"
                clos:class-slots
                clos:slot-definition-name
                clos:finalize-inheritance
                clos:class-finalized-p)
  #+Lispworks
  (:import-from "HCL"
                hcl:class-slots
                hcl:slot-definition-name)
  #+Lispworks
  (:import-from "CLOS"
                clos:finalize-inheritance
                clos:class-finalized-p)
  #+CLISP
  (:import-from "CLOS"
                clos::class-slots
                clos::slot-definition-name)
  #-(or Allegro CMUCL Lispworks CLISP)
  (error "Unsupported implementation.")
  (:export slot-definition-name
           class-slots
           finalize-inheritance
           class-finalized-p))

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(when (and (not (find-package 'clos))
           (find-package 'pcl))
  (rename-package (find-package 'pcl) 'pcl
                  `(clos ,@(package-nicknames 'pcl))))
