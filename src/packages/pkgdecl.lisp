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

;;; $Id: pkgdecl.lisp,v 1.24 2001/04/26 19:43:05 youngde Exp $

(in-package "CL-USER")

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (find-package 'clos))
             (find-package 'pcl))
    (rename-package (find-package 'pcl) 'pcl
                    `(clos ,@(package-nicknames 'pcl)))))

(defpackage "LISA"
  (:use "COMMON-LISP")
  (:shadow "ASSERT")
  (:export "DEFRULE" "DEFTEMPLATE" "ASSERT" "DEFIMPORT" "FACTS" "RULES"
           "AGENDA" "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY" "WATCH" "UNWATCH"
           "WATCHING" "HALT" "ASSERT-INSTANCE" "TELL-EXTERNALLY-MODIFIED"
           "TELL-LISA-MODIFIED-INSTANCE" "SLOT" "TEST" "ENGINE" "USE-ENGINE"
           "USE-DEFAULT-ENGINE" "WITH-INFERENCE-ENGINE" "=>"))

(defpackage "LISA-USER"
  (:use "COMMON-LISP")
  (:shadowing-import-from "LISA"
                          "ASSERT")
  (:import-from "LISA"
                "DEFRULE" "DEFTEMPLATE" "DEFIMPORT" "FACTS" "RULES"
                "AGENDA" "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY" "WATCH"
                "UNWATCH" "WATCHING" "HALT" "ASSERT-INSTANCE"
                "TELL-EXTERNALLY-MODIFIED" "TELL-LISA-MODIFIED-INSTANCE"
                "SLOT" "TEST" "ENGINE" "USE-ENGINE" "WITH-INFERENCE-ENGINE"
                "USE-DEFAULT-ENGINE" "=>"))

(defpackage "LISA.MULTIPROCESSING"
  (:use "COMMON-LISP")
  (:nicknames "LMP")
  (:import-from "PORT"
                "MAKE-PROCESS" "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT"
                "WITH-TIMEOUT" "PROCESS-YIELD" "KILL-PROCESS"
                "INTERRUPT-PROCESS" "RESTART-PROCESS" "PROCESSP"
                "PROCESS-NAME" "PROCESS-ACTIVE-P" "PROCESS-WHOSTATE"
                "CURRENT-PROCESS" "ALL-PROCESSES" "SHOW-PROCESSES")
  (:export "MAKE-PROCESS" "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT"
                "WITH-TIMEOUT" "PROCESS-YIELD" "KILL-PROCESS"
                "INTERRUPT-PROCESS" "RESTART-PROCESS" "PROCESSP"
                "PROCESS-NAME" "PROCESS-ACTIVE-P" "PROCESS-WHOSTATE"
                "CURRENT-PROCESS" "ALL-PROCESSES" "SHOW-PROCESSES"
                "MAKE-LOCK" "GET-LOCK" "GIVEUP-LOCK" "WITH-LOCK"
                "WITHOUT-SCHEDULING"))

(defpackage "LISA.REFLECT"
  (:use "COMMON-LISP")
  (:nicknames "REFLECT")
  #+(or Allegro CMU LispWorks)
  (:import-from "CLOS"
                "FINALIZE-INHERITANCE"
                "CLASS-FINALIZED-P")
  (:export "CLASS-SLOT-LIST"
           "FINALIZE-INHERITANCE"
           "CLASS-FINALIZED-P"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :lisa *features*))
