;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: load
;;; Description: Simple-minded loader for use in early development.

;;; $Id: load.lisp,v 1.29 2001/01/28 20:03:26 youngde Exp $

(in-package :user)

(defvar *lisa-root-pathname*
  (make-pathname :directory
                 (pathname-directory
                  (merge-pathnames *load-truename*
                                   *default-pathname-defaults*))))

(defvar *lisa-source-pathname*
  (make-pathname :directory
                 (append (pathname-directory *lisa-root-pathname*)
                         '("src"))))

(let ((files
       '(("packages" ("pkgdecl"))
         ("utils" ("utils" "compose"))
         ("engine" ("macros" "utils" "lisa-kb-class" "special-facts"
                    "strategies" "bindings" "token" "add-token" "fact"
                    "rete" "rete-compiler" "clear-token" "remove-token"
                    "token-tree" "node" "node1" "node1-tect" "node1-teq"
                    "node1-neq" "node1-tfn" "node1-rtl" "test" "node-test"
                    "test2-simple" "test2-eval" "node2" "node2-not"
                    "terminal-node" "slot" "pattern" "generic-pattern"
                    "not-pattern" "factories" "funcall" "rule" "parser"
                    "language" "activation" "environment" "watch" "debug")))))
  (labels ((load-files (path files)
             (cond ((null files)
                    (values t))
                   (t
                    (load (format nil "~A/~A" path (first files)))
                    (load-files path (rest files))))))
    (dolist (module files)
      (load-files
       (concatenate 'string
         (directory-namestring *lisa-source-pathname*)
         (first module)) (second module)))))
