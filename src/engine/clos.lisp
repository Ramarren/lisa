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
;;; support for CLOS reasoning.

;;; $Id: clos.lisp,v 1.1 2001/04/30 20:10:36 youngde Exp $

(in-package "LISA")

(defvar *clos-instance-map* (make-hash-table))
(defvar *clos-map-lock* (lmp:make-lock :name "CLOS Map Lock"))

(defstruct clos-instance-data
  (engine nil :type rete)
  (fact nil :type shadow-fact))

(defmacro with-clos-map-lock (&body body)
  `(lmp:with-lock (*clos-map-lock*) ,@body))

(defun get-clos-bindings (instance)
  (let ((bindings (gethash instance *clos-instance-map*)))
    (when (null bindings)
      (setf bindings (make-array 0 :initial-element nil
                                 :adjustable t
                                 :fill-pointer t))
      (setf (gethash instance *clos-instance-map*) bindings))
    (values bindings)))

(defun bind-clos-instance (engine instance fact)
  (with-clos-map-lock
      (vector-push-extend
       (make-clos-instance-data :engine engine :fact fact)
       (get-clos-bindings instance))))

(defun unbind-clos-instance (engine instance)
  (with-clos-map-lock
      (let* ((bindings (get-clos-bindings instance))
             (position (position engine bindings 
                                 :key #'clos-instance-data-engine)))
        (cl:assert (not (null position)) ()
          "LISA doesn't know about this CLOS instance: ~S." instance)
        (setf (aref bindings position) nil))))

(defun find-shadow-fact (engine instance &optional (errorp t))
  (with-clos-map-lock
      (let ((binding (find engine (get-clos-bindings instance)
                           :key #'clos-instance-data-engine)))
        (when (and (null binding) errorp)
          (error "LISA doesn't know about this CLOS instance: ~S." instance))
        (clos-instance-data-fact binding))))

(defun map-instances (instance func &rest args)
  (with-clos-map-lock
      (map nil #'(lambda (binding)
                   (unless (null binding)
                     (apply func 
                            (clos-instance-data-engine binding) instance args)))
           (get-clos-bindings instance))))


