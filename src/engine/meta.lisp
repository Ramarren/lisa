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

;;; File: meta.lisp
;;; Description: Meta operations that LISA uses to inspect fact classes.

;;; $Id: meta.lisp,v 1.7 2001/03/15 20:53:29 youngde Exp $

(in-package "LISA")

(defclass meta-fact ()
  ((symbolic-name :initarg :symbolic-name
                  :reader get-name)
   (class-name :initarg :class-name
               :reader get-class-name)
   (slots :initform (make-hash-table)
          :reader get-slots)))

(defmethod initialize-instance :after ((self meta-fact) &key slots)
  (let ((slot-table (get-slots self))
        (position -1))
    (mapc #'(lambda (slot-name)
              (setf (gethash slot-name slot-table)
                (make-slot-name slot-name (incf position))))
          slots)))

(defun make-meta-fact (name class-name slots)
  (make-instance 'meta-fact :symbolic-name name
                 :class-name class-name :slots slots))

(let ((meta-map (make-hash-table)))
  (defun register-class (name meta-object)
    (setf (gethash name meta-map) meta-object))

  (defun forget-registered-class (name)
    (remhash name meta-map))

  (defun forget-registered-classes ()
    (clrhash meta-map))

  (defun registered-classp (name)
    (gethash name meta-map))
  
  (defun find-registered-class (name)
    (let ((meta-object (gethash name meta-map)))
      (cl:assert (not (null meta-object)) ()
                 "Fact ~S does not have a registered class." name)
      (values meta-object))))

(defun import-and-register-class (symbolic-name real-name)
  (register-class symbolic-name (find-class real-name)))

(defun create-class-template (name slots)
  (let ((meta (make-meta-fact
               name (class-name (find-class 'deftemplate)) slots)))
    (register-class name meta)
    (values meta)))
  
