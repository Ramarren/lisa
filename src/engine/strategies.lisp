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
;;; resolution strategies for Lisa's RETE implementation. NB: The code here is
;;; raw and inefficient; it will change soon.

;;; $Id: strategies.lisp,v 1.7 2000/12/15 16:57:23 youngde Exp $

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

(defclass indexed-priority-list ()
  ((priority-list :reader get-priority-list)
   (index :initform nil
          :accessor get-index)
   (delta :accessor get-delta)
   (insertion-function :initarg :insertion-function
                       :reader get-insertion-function)
   (activations :initform (make-hash-table)
                :reader get-activations))
  (:documentation
   "Utility class that implements an indexed priority 'queue' to manage
   activations. Employed by various types of conflict resolution strategies,
   particularly DEPTH-FIRST-STRATEGY and BREADTH-FIRST-STRATEGY."))

(defmethod initialize-instance :after ((self indexed-priority-list)
                                       &key (priorities 500))
  (setf (slot-value self 'priority-list)
    (make-array (1+ priorities) :adjustable t :fill-pointer t))
  (setf (slot-value self 'delta (/ priorities 2))))

(defun insert-activation (plist activation)
  (with-accessors ((vector get-priority-list)
                   (index get-index)
                   (activations get-activations)) plist
    (let* ((salience (get-salience (get-rule activation)))
           (slot (+ salience (get-delta plist)))
           (queue (aref vector slot)))
      (setf (aref vector slot)
        (apply (get-insertion-function plist) activation queue))
      (index-salience)
      (setf (gethash (hash-code (get-token activation))) activation))))

(defclass depth-first-strategy (strategy)
  ()
  (:documentation
   "A depth-first conflict resolution strategy."))

(defmethod add-activation ((self depth-first-strategy) activation)
  (with-accessors ((activations get-activations)) self
    (setf activations
      (nconc activations `(,activation)))))

(defmethod retract-activation ((self depth-first-strategy) activation)
  (with-accessors ((activations get-activations)) self
    (setf activations
      (delete-if #'(lambda (act)
                     (= (hash-code act) (hash-code activation)))
                 activations))
    (values activation)))

(defmethod remove-activation ((self depth-first-strategy) token)
  (let ((activation
         (find-if #'(lambda (activation)
                      (= (hash-code activation) (hash-code token)))
                  (get-activations self))))
    (unless (null activation)
      (retract-activation self activation))))

(defmethod next-activation ((self depth-first-strategy))
  (pop (get-activations self)))

(defmethod remove-activations ((self depth-first-strategy))
  (setf (get-activations self) nil))

(defun make-depth-first-strategy ()
  (make-instance 'depth-first-strategy))

