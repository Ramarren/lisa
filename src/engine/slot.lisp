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

;;; File: slot.lisp
;;; Description: Represents a single slot within a pattern.

;;; $Id: slot.lisp,v 1.4 2000/12/04 16:44:22 youngde Exp $

(in-package :lisa)

(defclass slot ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (tests :initarg :tests
          :initform nil
          :reader get-tests))
  (:documentation
   "Represents a single slot within a pattern."))

(defmethod print-object ((self slot) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S)" (get-name self))))

(defun make-slot (name tests)
  (make-instance 'slot :name name :tests tests))

