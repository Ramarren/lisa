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

;;; File: environment.lisp
;;; Description: Defines the standard LISA environment.

;;; $Id: environment.lisp,v 1.9 2000/12/04 16:44:22 youngde Exp $

(in-package :lisa)

;;; CLASS-MAP maintains bindings between arbitrary names (symbols) and class
;;; names. LISA uses this map to locate class objects that represent facts in
;;; the knowledge base.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((class-map (make-hash-table)))
    (defun import-class (name class)
      (setf (gethash name class-map) (class-name class)))

    (defun find-imported-class (name &optional (errorp t))
      (let ((val (gethash name class-map)))
        (cond ((null val)
               (if errorp
                   (error "No imported class for symbol ~S." name)))
              (t
               (setf val (find-class val errorp))))
        (values val)))))

(defun import-and-register-class (name binding)
  (if (and (symbolp name)
           (symbolp binding))
      (import-class name (find-class binding))
    (error "IMPORT-AND-REGISTER-CLASS: both NAME and BINDING must be symbols.")))

(defgeneric current-engine ()
  (:documentation
   "Generic function used to locate the 'current' instance of the inference
   engine, whatever that means. The default implementation returns the
   singleton RETE; clients that employ more than one RETE instance should
   override this method."))

(let ((singleton-engine (make-rete)))
  (defmethod current-engine ()
    (values singleton-engine)))

(defmacro with-inference-engine ((engine) &body body)
  (let ((old-engine (gensym))
        (rval (gensym)))
    `(let ((,old-engine (use-engine ,engine))
           (,rval (progn ,@body)))
       (setf (current-engine) ,old-engine)
       (values ,rval))))


