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

;;; File: environment.lisp
;;; Description: Defines the standard LISA environment.

;;; $Id: environment.lisp,v 1.12 2001/03/14 18:54:36 youngde Exp $

(in-package :lisa)

(defgeneric current-engine ()
  (:documentation
   "Generic function used to locate the 'current' instance of the inference
   engine, whatever that means. The default implementation returns the
   singleton RETE; clients that employ more than one RETE instance should
   override this method."))

(let ((singleton-engine nil))
  (defmethod current-engine ()
    (when (null singleton-engine)
      (setf singleton-engine (make-rete)))
    (values singleton-engine)))

(defmacro with-inference-engine ((engine) &body body)
  (let ((old-engine (gensym))
        (rval (gensym)))
    `(let ((,old-engine (use-engine ,engine))
           (,rval (progn ,@body)))
       (setf (current-engine) ,old-engine)
       (values ,rval))))


