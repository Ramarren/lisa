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

;;; $Id: pkgdecl.lisp,v 1.8 2001/03/15 16:00:31 youngde Exp $

(in-package "CL-USER")

(defpackage "LISA"
  (:use "COMMON-LISP")
  (:shadow "ASSERT")
  (:export "DEFRULE" "DEFTEMPLATE" "ASSERT" "DEFIMPORT" "FACTS" "RULES"
           "AGENDA" "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY" "WATCH" "UNWATCH"
           "WATCHING" "HALT"))

;;; accommodate implementations whose CLOS is really PCL, like CMUCL...

(when (and (not (find-package 'clos))
           (find-package 'pcl))
  (rename-package (find-package 'pcl) 'pcl
                  `(clos ,@(package-nicknames 'pcl))))
