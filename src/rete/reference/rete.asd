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

;;; File: rete.asd
;;; Description: System definition file.

;;; $Id: rete.asd,v 1.1 2004/06/07 16:06:55 youngde Exp $

(in-package :cl-user)

(asdf:defsystem :lisa.rete
  :components ((:file "node-tests")
               (:file "shared-node")
               (:file "successor")
               (:file "node-pair")
               (:file "terminal-node")
               (:file "node1")
               (:file "join-node")
               (:file "node2")
               (:file "node2-not")
               (:file "node2-test")
               (:file "node2-exists")
               (:file "rete-compiler")
               (:file "tms"
                :depends-on ("join-node"))
               (:file "network-ops")
               (:file "network-crawler"))
  :depends-on ("lisa.core"))
