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

;;; File: lisa-server.lisp
;;; Description: A sample implementation of an RPC server capable of reasoning
;;; over remote objects.

;;; $Id: lisa-server.lisp,v 1.1 2002/12/11 19:30:39 youngde Exp $

(in-package "CL-USER")

(defvar *lisa-server-host* "localhost")
(defvar *lisa-server-port* 10000)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require 'aclrpc)
  (unless (find-package "LISA.SERVER")
    (defpackage "LISA.SERVER"
      (:use "LISA-LISP" "NET.RPC"))))

(in-package "LISA.SERVER")

(defvar *server-proc* nil)

(defclass remote-frodo (rpc-remote-ref) ())

(defun initialize-client-environment (port)
  (format t "Initialising client environment~%")
  (import-remote-class port 'remote-frodo "frodo"))

(defun make-server ()
  (make-rpc-server
   'rpc-socket-server
   :name "LISA RPC Server"
   :local-port *server-port*
   :open :listener
   :connect-action :call
   :connect-function
   #'(lambda (port &rest args)
       (initialize-client-environment port)
       (values))))

(defun start-server ()
  (when (null *server-proc*)
    (setf *server-proc* (make-server)))
  *server-proc*)

(defun stop-server ()
  (unless (null *server-proc*)
    (rpc-close :stop :final)
    (setf *server-proc* nil)))

