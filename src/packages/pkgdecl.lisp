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

;;; $Id: pkgdecl.lisp,v 1.46 2002/07/29 17:24:56 youngde Exp $

(in-package "CL-USER")

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (find-package 'clos))
             (find-package 'pcl))
    (rename-package (find-package 'pcl) 'pcl
                    `(clos ,@(package-nicknames 'pcl)))))

(defpackage "LISA"
  (:use "COMMON-LISP")
  (:export "USE-LISA" "DEFRULE" "DEFTEMPLATE" "ASSERT" "DEFIMPORT" "FACTS"
           "RULES" "AGENDA" "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY" "WATCH"
           "UNWATCH" "WATCHING" "HALT" "ASSERT-INSTANCE" "RETRACT-INSTANCE"
           "MARK-INSTANCE-AS-CHANGED" "SLOT" "TEST" "ENGINE" "USE-ENGINE"
           "USE-DEFAULT-ENGINE" "CURRENT-ENGINE" "WITH-INFERENCE-ENGINE"
           "MAKE-INFERENCE-ENGINE" "GET-NAME" "RULE" "ASSERT-FROM-STRING" "=>"
           "DEFFACTS" "*SHOW-LISA-WARNINGS*" "UNDEFRULE" "RETRIEVE"
           "FORGET-QUERY" "DEFAULT" "INITIAL-FACT" "WITH-SIMPLE-QUERY" "WALK")
  (:shadow "ASSERT"))

(defpackage "LISA-USER"
  (:use "COMMON-LISP")
  (:shadowing-import-from "LISA" "ASSERT" "DEFAULT")
  (:import-from "LISA"
                "DEFRULE" "DEFTEMPLATE" "DEFIMPORT" "FACTS"
                "RULES" "AGENDA" "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY"
                "WATCH" "UNWATCH" "WATCHING" "HALT" "ASSERT-INSTANCE"
                "RETRACT-INSTANCE" "MARK-INSTANCE-AS-CHANGED"
                "SLOT" "TEST" "ENGINE" "USE-ENGINE" "USE-DEFAULT-ENGINE"
                "CURRENT-ENGINE" "WITH-INFERENCE-ENGINE"
                "MAKE-INFERENCE-ENGINE" "GET-NAME" "RULE" "ASSERT-FROM-STRING"
                "=>" "DEFFACTS" "*SHOW-LISA-WARNINGS*" "UNDEFRULE" "RETRIEVE"
                "FORGET-QUERY" "INITIAL-FACT" "WITH-SIMPLE-QUERY" "WALK")) 

(defpackage "LISA.REFLECT"
  (:use "COMMON-LISP")
  (:nicknames "REFLECT")
  #+(or Allegro LispWorks)
  (:import-from "CLOS"
                "FINALIZE-INHERITANCE"
                "ENSURE-CLASS"
                "CLASS-DIRECT-SUPERCLASSES"
                "CLASS-FINALIZED-P")
  #+CMU
  (:import-from "CLOS"
                "FINALIZE-INHERITANCE"
                "CLASS-FINALIZED-P")
  (:export "CLASS-SLOT-LIST"
           "FINALIZE-INHERITANCE"
           "ENSURE-CLASS"
           "CLASS-FINALIZED-P"
           "FIND-DIRECT-SUPERCLASSES"
           "CLASS-ALL-SUPERCLASSES"))

(defpackage "LISA.UTILS"
  (:use "COMMON-LISP")
  (:nicknames "UTILS")
  (:export "FIND-BEFORE"
           "FIND-AFTER"
           "FIND-IF-AFTER"
           "LSTHASH"
           "COLLECT"
           "FLATTEN"
           "MAP-IN"
           "STRING-TOKENS"
           "COMPOSE"
           "COMPOSE-F"
           "COMPOSE-ALL"))

;;; This macro is courtesy of Paul Werkowski. A very nice idea.

(defmacro define-lisa-lisp ()
  (flet ((externals-of (pkg)
           (loop for s being each external-symbol in pkg collect s)))
    (let* ((lisa-externs (externals-of "LISA"))
           (lisa-shadows (intersection (package-shadowing-symbols "LISA")
                                       lisa-externs))
           (cl-externs (externals-of "COMMON-LISP")))
      `(defpackage "LISA-LISP"
         (:use "COMMON-LISP")
         (:shadowing-import-from "LISA" ,@lisa-shadows)
         (:import-from "LISA" ,@(set-difference lisa-externs lisa-shadows))
         (:export ,@cl-externs)
         (:export ,@lisa-externs)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-lisa-lisp)
  (pushnew :lisa *features*))
