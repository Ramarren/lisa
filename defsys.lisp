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

;;; File: defsys.lisp
;;; Description: System definition file for LISA project.
;;;
;;; $Id: defsys.lisp,v 1.48 2001/04/24 20:37:13 youngde Exp $

(in-package "CL-USER")

(defvar *lisa-root-pathname*
  (make-pathname :directory
                 (pathname-directory
                  (merge-pathnames *load-truename*
                                   *default-pathname-defaults*))))

(defun make-lisa-path (relative-path)
  (concatenate 'string (directory-namestring *lisa-root-pathname*)
               relative-path))

(setf (logical-pathname-translations "lisa")
  `(("src;**;" ,(make-lisa-path "src/**/"))
    ("lib;**;*.*" ,(make-lisa-path "lib/**/"))
    ("contrib;**;" ,(make-lisa-path "contrib/**/"))
    ("clocc;port;**;*.*" ,(make-lisa-path "contrib/clocc/port/**/"))))

(setf (logical-pathname-translations "clocc")
  `(("src;port;**;*.*" ,(translate-logical-pathname
                         "lisa:clocc;port;**;*.*"))))

(defvar *lisa-source-pathname*
  (translate-logical-pathname "lisa:src;"))

(defvar *lisa-binary-pathname*
  #+Allegro
  (translate-logical-pathname "lisa:lib;acl;")
  #+LispWorks
  (translate-logical-pathname "lisa:lib;lispworks;")
  #+CMU
  (translate-logical-pathname "lisa:lib;cmucl;")
  #+CLISP
  (translate-logical-pathname "lisa:lib;clisp;")
  #-(or Allegro LispWorks CMU CLISP)
  (error "Unsupported implementation."))

(load "lisa:clocc;port;port.system")

(mk:defsystem :lisa
    :source-pathname *lisa-source-pathname*
    :binary-pathname *lisa-binary-pathname*
    :source-extension "lisp"
    :components ((:module "packages"
                          :source-pathname "packages"
                          :binary-pathname "packages"
                          :components ((:file "pkgdecl")))
                 (:module "reflection"
                          :source-pathname "reflect"
                          :binary-pathname "reflect"
                          :components ((:file "reflect")))
                 (:module "utils"
                          :source-pathname "utils"
                          :binary-pathname "utils"
                          :components ((:file "compose")
                                       (:file "utils"))
                          :depends-on (packages))
                 (:module "lisa-macros"
                          :source-pathname "engine"
                          :binary-pathname "engine"
                          :components ((:file "macros"))
                          :depends-on (packages))
                 (:module "preamble"
                          :source-pathname "engine"
                          :binary-pathname "engine"
                          :components ((:file "generics")
                                       (:file "preamble"))
                          :depends-on (packages))
                 (:module "conditions"
                          :source-pathname "engine"
                          :binary-pathname "engine"
                          :components ((:file "conditions"))
                          :depends-on (packages))
                 (:module "engine"
                          :source-pathname "engine"
                          :binary-pathname "engine"
                          :components ((:file "utils")
                                       (:file "deftemplate")
                                       (:file "slot-name")
                                       (:file "meta")
                                       (:file "strategies")
                                       (:file "bindings")
                                       (:file "token")
                                       (:file "add-token")
                                       (:file "fact")
                                       (:file "shadow-fact"
                                              :depends-on (fact))
                                       (:file "rete")
                                       (:file "clear-token"
                                              :depends-on (token))
                                       (:file "remove-token"
                                              :depends-on (token))
                                       (:file "token-tree")
                                       (:file "node")
                                       (:file "node1"
                                              :depends-on (node))
                                       (:file "node1-tect"
                                              :depends-on (node1))
                                       (:file "node1-nop"
                                              :depends-on (node1))
                                       (:file "node1-tfn"
                                              :depends-on (node1))
                                       (:file "node1-teq"
                                              :depends-on (node1))
                                       (:file "node1-neq"
                                              :depends-on (node1))
                                       (:file "node1-rtl"
                                              :depends-on (node1))
                                       (:file "test")
                                       (:file "node-test")
                                       (:file "test2-eq")
                                       (:file "test2-neq")
                                       (:file "test2-eval")
                                       (:file "node2"
                                              :depends-on (node token-tree))
                                       (:file "node2-not"
                                              :depends-on (node2))
                                       (:file "terminal-node"
                                              :depends-on (node))
                                       (:file "slot")
                                       (:file "pattern")
                                       (:file "generic-pattern"
                                              :depends-on (pattern))
                                       (:file "not-pattern"
                                              :depends-on (generic-pattern))
                                       (:file "bound-pattern"
                                              :depends-on (generic-pattern))
                                       (:file "test-pattern"
                                              :depends-on (pattern))
                                       (:file "rete-compiler"
                                              :depends-on
                                              (test-pattern generic-pattern
                                              not-pattern pattern))
                                       (:file "factories")
                                       (:file "funcall")
                                       (:file "rule"
                                              :depends-on
                                              (test-pattern generic-pattern
                                              not-pattern pattern))
                                       (:file "parser")
                                       (:file "language"
                                              :depends-on (parser))
                                       (:file "activation")
                                       (:file "environment")
                                       (:file "watch")
                                       (:file "debug")
                                       (:file "instrumenting"))
                          :depends-on (packages reflection conditions utils
                                                lisa-macros preamble)))
    :depends-on (port)
    :initially-do
    (progn
      (mk:system-source-size :lisa :all)
      (mk:system-source-size :lisa :new-source-and-dependents)))

(defmacro with-quiet-compile (&body body)
  `(let ((*compile-print* nil))
    ,@body))

(defun compile-lisa ()
  (with-quiet-compile
      (mk:compile-system :lisa)))

(defun load-lisa ()
  (with-quiet-compile
      (mk:load-system :lisa)))

(defun clean-lisa ()
  (mk:clean-system :lisa))

(defun optimize-lisa ()
  (proclaim '(optimize (speed 3) (safety 1) (debug 1))))
