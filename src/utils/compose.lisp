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

;;; File: compose.lisp
;;; Description: Utilities used to compose anonymous functions.
;;;
;;; $Id: compose.lisp,v 1.3 2000/11/30 15:59:40 youngde Exp $

(in-package :lisa)

(defun build-lambda-expression (forms)
  (labels ((compose-body (forms &optional (body nil))
             (if (null forms)
                 body
               (compose-body (rest forms)
                             (nconc body
                                    `(,(first forms)))))))
    `(lambda ()
       (progn ,@(compose-body forms)))))
  
(defmacro compile-function (forms)
  "Build and compile an anonymous function, using the body provided in
  FORMS."
  `(compile nil (build-lambda-expression ,forms)))
