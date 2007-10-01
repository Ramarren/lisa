;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

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

;;; Description: Convenience interface for loading Lisa from scratch.

;;; $Id: install.lisp,v 1.9 2007/10/01 13:47:37 youngde Exp $

(in-package :cl-user)

(defvar *install-root* (make-pathname :directory (pathname-directory *load-truename*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :asdf)
    (load (merge-pathnames "misc/asdf" *install-root*))))

(push *install-root* asdf:*central-registry*)
(asdf:operate 'asdf:load-op :lisa :force t)

