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

;;; File: lisa-client.lisp
;;; Description: A sample implementation of an RPC client that requests
;;; inferencing services from a LISA server.

;;; $Id: lisa-client.lisp,v 1.1 2002/12/11 19:30:38 youngde Exp $

(in-package "CL-USER")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require 'aclrpc)
  (unless (find-package "LISA.CLIENT")
    (defpackage "LISA.CLIENT"
      (:use "LISA-LISP" "NET.RPC")
      (:import "CL-USER::*LISA-SERVER-HOST*"
               "CL-USER::*LISA-SERVER-PORT*"))))

(in-package "LISA.CLIENT")

(defclass frodo ()
  ((name :initarg :name
         :initform nil
         :accessor frodo-name)
   (has-ring :initform nil
             :accessor frodo-has-ring)
   (companions :initform nil
               :accessor frodo-companions)))

(defmethod print-object ((self frodo) strm)
  (print-unreadable-object (strm strm :type t :identity t)
    (format strm "~S, ~S, ~S" 
            (frodo-name self)
            (frodo-has-ring self)
            (frodo-companions self))))

(defun make-client ()
  (make-rpc-client
   'rpc-socket-port
   :remote-host *lisa-server-host*
   :remote-port *lisa-server-port*))
   
(defun run-client ()
  (let ((frodo (make-instance-frodo :name 'frodo)))
    (format t "Frodo instance before inferencing: ~S~%" frodo)
    (multiple-value-bind (port stuff)
        (make-client)
      (with-remote-port (port :close t)
        (rcall 'reset)
        (rcall 'assert-instance frodo)
        (rcall 'run)
        (format t "Frodo instance after inferencing: ~S~%" frodo)
        frodo))))
