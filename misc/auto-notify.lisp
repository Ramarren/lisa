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

;;; File: auto-notify.lisp
;;; Description: Small test code for LISA's auto-notify feature.

;;; $Id: auto-notify.lisp,v 1.2 2002/11/25 20:56:47 youngde Exp $

(in-package "LISA-USER")

(defclass frodo (standard-kb-object)
  ((name :initarg :name
         :initform 'frodo
         :reader frodo-name)
   #+Allegro
   (has-ring :initform nil
             :accessor has-ring)
   #+Lispworks
   (has-ring :initform nil
             :reader has-ring)))

#+Lispworks
(defsetf has-ring (frodo) (new-value)
  `(setf (slot-value ,frodo 'has-ring) ,new-value))

(defrule frodo ()
  (frodo (has-ring t))
  =>
  (format t "Frodo has the Ring!~%"))

(defparameter *frodo* (make-instance 'frodo))

(reset)

(assert ((identity *frodo*)))
