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

;;; $Id: meta.lisp,v 1.3 2001/03/14 18:54:36 youngde Exp $

(in-package :lisa)

;;; CLASS-MAP maintains bindings between arbitrary names (symbols) and class
;;; names. LISA uses this map to locate class objects that represent facts in
;;; the knowledge base.

(let ((class-map (make-hash-table)))
  (defun register-class (name class)
    (setf (gethash name class-map) (class-name class)))

  (defun forget-registered-class (name)
    (remhash name class-map))

  (defun forget-registered-classes ()
    (clrhash class-map))

  (defun registered-classp (name)
    (gethash name class-map))
  
  (defun find-registered-class (name)
    (let ((real-name (gethash name class-map)))
      (cl:assert (not (null real-name)) ()
                 "Fact ~S does not have a registered class." name)
      (find-class real-name))))

(defun import-and-register-class (symbolic-name real-name)
  (register-class symbolic-name (find-class real-name)))

(defun create-class-template (name slots)
  (let ((template
         (eval `(defclass ,name (deftemplate) (,@slots)))))
    (clos:finalize-inheritance template)
    (register-class name template)
    (values template)))
  
