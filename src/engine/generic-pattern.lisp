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

;;; File: generic-pattern.lisp
;;; Description: Class representing the default style of pattern found
;;; on rule LHSs, as in (fact (slot-0 1) (slot-1 blue)).

;;; $Id: generic-pattern.lisp,v 1.3 2000/11/15 16:34:34 youngde Exp $

(in-package :lisa)

(defclass generic-pattern (pattern)
  ()
  (:documentation
   "Represents  the default style of pattern found on rule LHSs, as in
   (fact (slot-0 1) (slot-1 blue))."))

(defun make-generic-pattern (head body)
  "Constructor for the GENERIC-PATTERN class."
  (let ((pattern (make-instance 'generic-pattern :name head)))
    (mapc #'(lambda (slot)
              (add-slot pattern (first slot)
                        (make-test1 (second slot))))
          body)
    (values pattern)))

