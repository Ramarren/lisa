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

;;; File: strategies.lisp
;;; Description: Classes that implement the various default conflict
;;; resolution strategies for Lisa's RETE implementation.

;;; $Id: strategies.lisp,v 1.5 2000/11/27 21:28:50 youngde Exp $

(in-package :lisa)

(defclass strategy ()
  ((activations :initarg :activations
                :initform nil
                :accessor get-activations))
  (:documentation
   "Serves as the base class for all classes implementing conflict
   resolution strategies."))

(defgeneric add-activation (strategy activation))
(defgeneric remove-activation (strategy token))
(defgeneric find-activation (strategy token rule))
(defgeneric next-activation (strategy))
(defgeneric remmove-activations (strategy))

(defclass depth-first-strategy (strategy)
  ()
  (:documentation
   "A depth-first conflict resolution strategy."))

(defmethod add-activation ((self depth-first-strategy) activation)
  (with-accessors ((activations get-activations)) self
    (setf activations
      (nconc activations `(,activation)))))

(defmethod remove-activation ((self depth-first-strategy) token)
  (with-accessors ((activations get-activations)) self
    (setf activations
      (delete-if #'(lambda (tok)
                     (= (hash-code tok) (hash-code token)))
                 activations))))

(defmethod next-activation ((self depth-first-strategy))
  (pop (get-activations self)))

(defmethod remove-activations ((self depth-first-strategy))
  (setf (get-activations self) nil))

(defun make-depth-first-strategy ()
  (make-instance 'depth-first-strategy))

