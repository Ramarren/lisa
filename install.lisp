;;; -*- Mode: Lisp -*-

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

;;; File: install.lisp
;;; Description: 

;;; $Id: install.lisp,v 1.2 2004/06/07 18:35:17 youngde Exp $

(in-package :cl-user)

(defvar *lisa-root-pathname*
    (make-pathname :directory
                   (pathname-directory *load-truename*)
                   :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)))

(defun make-lisa-path (relative-path)
  (concatenate 'string (namestring *lisa-root-pathname*)
               relative-path))

(setf (logical-pathname-translations "lisa")
  `(("src;**;" ,(make-lisa-path "src/**/"))
    ("lib;**;*.*" ,(make-lisa-path "lib/**/"))
    ("config;*.*" ,(make-lisa-path "config/"))
    ("debugger;*.*" ,(make-lisa-path "src/debugger/"))
    ("contrib;**;" ,(make-lisa-path "contrib/**/"))))

(defun lisa-debugger ()
  (translate-logical-pathname "lisa:debugger;lisa-debugger.lisp"))

;;; Sets up the environment so folks can use the non-portable form of REQUIRE
;;; with some implementations...

#+Allegro
(setf system:*require-search-list*
  (append system:*require-search-list*
          `(:newest ,(lisa-debugger))))

#+clisp
(setf custom:*load-paths*
  (append custom:*load-paths* `(,(lisa-debugger))))

#+LispWorks
(let ((loadable-modules `(("lisa-debugger" . ,(lisa-debugger)))))

  (defadvice (require lisa-require :around)
      (module-name &optional pathname)
    (let ((lisa-module
           (find module-name loadable-modules
                 :test #'string=
                 :key #'car)))
      (if (null lisa-module)
          (call-next-advice module-name pathname)
        (call-next-advice module-name (cdr lisa-module))))))

#-:asdf
(load (merge-pathnames
       (make-pathname
        :directory '(:relative "misc")
        :name "asdf" :type "lisp" :case :local)
       *load-truename*))

(eval-when (:load-toplevel :execute)
  (flet ((find-or-load-system (system path)
           (let ((path (merge-pathnames path *load-truename*)))
             (unless (asdf:find-system system nil)
               (load path)))))
    (find-or-load-system :lisa.packages
                         (make-pathname
                          :directory '(:relative "src" "packages")
                          :name "packages" :type "asd" :case :local))
    (find-or-load-system :lisa.implementations
                         (make-pathname
                          :directory '(:relative "src" "implementations")
                          :name "implementations" :type "asd" :case :local))
    (find-or-load-system :lisa.utils
                         (make-pathname
                          :directory '(:relative "src" "utils")
                          :name "utils" :type "asd" :case :local))
    (find-or-load-system :lisa.reflect
                         (make-pathname
                          :directory '(:relative "src" "reflect")
                          :name "reflect" :type "asd" :case :local))
    (find-or-load-system :lisa.core
                         (make-pathname
                          :directory '(:relative "src" "2.0-core")
                          :name "core" :type "asd" :case :local))
    (find-or-load-system :lisa.rete
                         (make-pathname
                          :directory '(:relative "src" "rete" "reference")
                          :name "rete" :type "asd" :case :local))
    (find-or-load-system :lisa.config
                         (make-pathname
                          :directory '(:relative "src" "config")
                          :name "config" :type "asd" :case :local))
    (asdf:operate 'asdf:load-op :lisa)))
  
