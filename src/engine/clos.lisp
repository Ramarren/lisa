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

;;; File: clos.lisp
;;; Description: Contains code and data structures that provide some ancillary
;;; support for CLOS reasoning. You'll find this stuff globally (rather than
;;; within, say, class RETE) because LISA assumes responsibility for managing
;;; the instance->engine bindings, which are especially needed whenever a CLOS
;;; object is modified outside of LISA's control (triggering the notification
;;; protocol). This is especially convenient when the application is running
;;; multiple inference engines and a single CLOS instance can reside in more
;;; than one.

;;; $Id: clos.lisp,v 1.4 2001/05/01 19:52:37 youngde Exp $

(in-package "LISA")

(defvar *clos-instance-map* (make-hash-table))
(defvar *clos-map-lock* (lmp:make-lock :name "CLOS Map Lock"))

(defstruct clos-instance-data
  (engine nil :type rete)
  (fact nil :type shadow-fact))

(defmacro with-clos-map-lock (&body body)
  `(lmp:with-lock (*clos-map-lock*) ,@body))

(defmacro with-writeable-clos-bindings ((bindings instance) &body body)
  (let ((rval (gensym)))
    `(with-clos-map-lock
      (let* ((,bindings (gethash ,instance *clos-instance-map*))
             (,rval (progn ,@body)))
        (if (null ,bindings)
            (remhash ,instance *clos-instance-map*)
          (setf (gethash ,instance *clos-instance-map*) ,bindings))
        (values ,rval)))))

(defmacro with-readonly-clos-bindings ((bindings instance) &body body)
  `(with-clos-map-lock
    (let ((,bindings (gethash ,instance *clos-instance-map*)))
      (progn ,@body))))

(defun bind-clos-instance (engine instance fact)
  (with-writeable-clos-bindings (bindings instance)
    (setf bindings
      (push (make-clos-instance-data :engine engine :fact fact) bindings))))

(defun unbind-clos-instance (engine instance)
  (with-writeable-clos-bindings (bindings instance)
    (setf bindings
      (delete engine bindings :key #'clos-instance-data-engine))))

(defun find-shadow-fact (engine instance &optional (errorp t))
  (with-readonly-clos-bindings (bindings instance)
    (let ((binding (find engine bindings
                         :key #'clos-instance-data-engine)))
        (when (and (null binding) errorp)
          (error "LISA doesn't know about this CLOS instance: ~S." instance))
        (clos-instance-data-fact binding))))

(defun map-clos-instances (instance func &rest args)
  (with-readonly-clos-bindings (bindings instance)
    (mapc #'(lambda (binding)
              (apply func 
                     (clos-instance-data-engine binding) instance args))
          bindings)))


