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

;;; File: bindings.lisp
;;; Description: Classes in this file represent various types of
;;; variable bindings that form the lexical environment of rule
;;; right-hand-sides.

;;; $Id: bindings.lisp,v 1.3 2000/12/06 17:13:06 youngde Exp $

(in-package :lisa)

(defclass binding ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (location :initarg :location
             :initform nil
             :reader get-location))
  (:documentation
   "The base class for all types of bindings."))

(defmethod print-object ((self binding) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; location = ~D)"
            (get-name self) (get-location self))))

(defclass pattern-binding (binding)
  ()
  (:documentation
   "Represents a fact-assignment binding."))

(defun make-pattern-binding (name location)
  (make-instance 'pattern-binding :name name :location location))

(defclass slot-binding (binding)
  ((slot-name :initarg :slot-name
              :reader get-slot-name))
  (:documentation
   "Represents variable bindings that can occur within pattern slots."))

(defmethod print-object ((self slot-binding) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; location = ~D ; slot-name = ~S)"
            (get-name self) (get-location self) (get-slot-name self))))

(defun make-slot-binding (binding-name location slot-name)
  (make-instance 'slot-binding :name binding-name
                 :location location :slot-name slot-name))
