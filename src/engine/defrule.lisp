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
;;;
;;; File: defrule.lisp
;;; Description: The DEFRULE class.
;;;
;;; $Id: defrule.lisp,v 1.2 2000/10/17 02:08:22 youngde Exp $

(in-package "LISA")

(defclass defrule ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (comment :initform nil
            :initarg :comment
            :accessor get-comment)
   (salience :initform 0
             :initarg :salience
             :accessor get-salience)
   (patterns :initform nil
             :accessor get-patterns)
   (actions :initform nil
            :accessor get-actions))
  (:documentation
   "This class represents LISA rules."))

(defmethod add-pattern ((rule defrule) pattern)
  (with-accessors ((patterns get-patterns)) rule
    (setf patterns (nconc patterns pattern))))

(defmethod set-actions ((rule defrule) &rest actions)
  (setf (get-actions rule)
        #'(lambda ()
            `,@actions)))

(defun make-defrule (name)
  "Constructor for class DEFRULE."
  (make-instance 'defrule :name name))
