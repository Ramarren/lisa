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

;;; $Id: environment.lisp,v 1.3 2000/11/16 19:07:45 youngde Exp $

(in-package :lisa)

(let ((class-map (make-hash-table)))
  (defun import-class (name class)
    (setf (gethash name class-map) (class-name class)))

  (defun find-imported-class (name)
    (let ((val (gethash name class-map)))
      (if (null val)
          (error "No imported class for symbol ~S." name)
        (find-class val)))))

(let ((engine (make-rete)))
  (defun current-engine ()
    (values engine))
  (defun (setf current-engine) (new-engine)
    (setf engine new-engine))
  (defun use-engine (new-engine)
    (let ((old-engine (current-engine)))
      (setf (current-engine) new-engine)
      (values old-engine))))

(defmacro with-inference-engine ((engine) &body body)
  (let ((old-engine (gensym))
        (rval (gensym)))
    `(let ((,old-engine (use-engine ,engine))
           (,rval (progn ,@body)))
       (setf (current-engine) ,old-engine)
       (values ,rval))))


