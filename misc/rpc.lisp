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

;;; File: rpc.lisp
;;; Description:

;;; $Id: rpc.lisp,v 1.4 2002/12/10 16:31:37 youngde Exp $

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'aclrpc)
  (unless (find-package "RPC")
    (defpackage "RPC"
      (:use "COMMON-LISP" "NET.RPC"))))

(in-package "RPC")

(defvar *server-host* "localhost")
(defvar *server-port* 10000)
(defvar *server-proc* nil)

(defclass frodo ()
  ((name :initarg :name
         :initform nil
         :accessor frodo-name)
   (age :initform 0
        :accessor frodo-age)))

(defclass remote-frodo (rpc-remote-ref) ())

(defmethod frodo-name ((self remote-frodo))
  (rcall 'frodo-name self))

(defmethod (setf frodo-age) (new-value (self remote-frodo))
  (rcall '|SETF FRODO-AGE| new-value self))

(defun assert-instance (object)
  (format t "Frodo's name: ~S~%" (frodo-name object))
  (setf (frodo-age object) 100)
  object)

(defun initialize-client-environment (port)
  (pprint "initialising client environment")
  (terpri)
  (import-remote-class port 'remote-frodo "frodo"))

(defun make-server ()
  (make-rpc-server
   'rpc-socket-server
   :name "RPC Server"
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

(defun make-client ()
  (make-rpc-client
   'rpc-socket-port
   :remote-host *server-host*
   :remote-port *server-port*))
   
(defun run-client ()
  (multiple-value-bind (port stuff)
      (make-client)
    (with-remote-port (port :close t)
      (rcall 'assert-instance (make-instance 'frodo :name 'frodo)))))
