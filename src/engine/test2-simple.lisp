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

;;; File: test2-simple.lisp
;;; Description: This class holds an individual test performed by
;;; two-input nodes.

;;; $Id: test2-simple.lisp,v 1.1 2000/12/05 16:02:36 youngde Exp $

(in-package :lisa)

(defclass test2-simple ()
  ((fact-location :initarg :fact-location
                  :reader get-fact-location)
   (left-slot-name :initarg :left-slot-name
                   :reader get-left-slot-name)
   (right-slot-name :initarg :right-slot-name
                    :reader get-right-slot-name))
  (:documentation
   "This class holds an individual test performed by two-input nodes."))

(defun make-test2-simple (fact-location left-slot right-slot)
  (make-instance 'test2-simple :fact-location fact-location
                 :left-slot-name left-slot
                 :right-slot-name right-slot))
