;;; -*- mode: Lisp -*-
	
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

;;; File: core.asd
;;; Description: System definition file.

;;; $Id: core.asd,v 1.1 2004/06/07 15:47:47 youngde Exp $

(in-package :cl-user)

(asdf:defsystem :lisa-core
  :components (:components ((:file "deffacts")
                            (:file "fact")
                            (:file "watches")
                            (:file "activation")
                            (:file "strategies")
                            (:file "context")
                            (:file "rule")
                            (:file "pattern")
                            (:file "parser"
                             :depends-on ("pattern"))
                            (:file "language"
                             :depends-on ("parser"))
                            (:file "tms-support")
                            (:file "rete"
                             :depends-on ("language" "tms-support"))
                            (:file "meta")
                            (:file "binding")
                            (:file "token")
                            (:file "retrieve"))
               :depends-on ("packages" "reflect" "utils" "preamble")))

