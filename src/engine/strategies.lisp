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

;;; $Id: strategies.lisp,v 1.16 2001/01/23 21:05:00 youngde Exp $

(in-package :lisa)

(defclass strategy ()
  ((activations :initarg :activations
                :initform nil
                :accessor get-activations))
  (:documentation
   "Serves as the base class for all classes implementing conflict
   resolution strategies."))

(defgeneric add-activation (strategy activation))
(defgeneric find-activation (strategy token))
(defgeneric next-activation (strategy))
(defgeneric remove-activations (strategy))
(defgeneric list-activations (strategy))

(defclass indexed-priority-list ()
  ((priority-vector :reader get-priority-vector)
   (inodes :initform nil
           :accessor get-inodes)
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
  (setf (slot-value self 'priority-vector)
    (make-array (1+ priorities) :initial-element nil))
  (setf (slot-value self 'delta) (/ priorities 2)))

(defun insert-activation (plist activation)
  (declare (type indexed-priority-list plist))
  (flet ((index-salience (priority)
           (declare (type fixnum priority))
           (with-accessors ((inodes get-inodes)) plist
             (setf inodes
               (sort (pushnew priority inodes)
                     #'(lambda (p1 p2) (> p1 p2)))))))
    (with-accessors ((vector get-priority-vector)
                     (activations get-activations)) plist
    (let* ((salience (get-salience (get-rule activation)))
           (inode (+ salience (get-delta plist)))
           (queue (aref vector inode)))
      (when (null queue)
        (index-salience inode))
      (setf (aref vector inode)
        (apply (get-insertion-function plist)
               `(,activation ,queue)))
      (setf (gethash (hash-code activation) activations) activation)))))

(defun lookup-activation (plist token)
  (declare (type indexed-priority-list plist))
  (gethash (hash-code token) (get-activations plist)))

(defun get-next-activation (plist)
  (declare (type indexed-priority-list plist))
  (with-accessors ((inodes get-inodes)
                   (vector get-priority-vector)
                   (activations get-activations)) plist
    (let ((inode (first inodes)))
      (cond ((null inode)
             (values nil))
            (t
             (let ((activation (pop (aref vector inode))))
               (when (null (aref vector inode))
                 (pop inodes))
               (remhash (hash-code activation) activations)
               (values activation)))))))

(defun get-all-activations (plist)
  (declare (type indexed-priority-list plist))
  (let ((list nil))
    (maphash #'(lambda (key activation)
                 (when (eligible-p activation)
                   (setf list (nconc list `(,activation)))))
             (get-activations plist))
    (values list)))

(defun make-indexed-priority-list (func)
  (make-instance 'indexed-priority-list :insertion-function func))

(defclass depth-first-strategy (strategy)
  ((priority-queue :reader get-priority-queue))
  (:documentation
   "A depth-first conflict resolution strategy."))

(defmethod add-activation ((self depth-first-strategy) activation)
  (insert-activation (get-priority-queue self) activation))

(defmethod find-activation ((self depth-first-strategy) token)
  (lookup-activation (get-priority-queue self) token))

(defmethod next-activation ((self depth-first-strategy))
  (get-next-activation (get-priority-queue self)))

(defmethod remove-activations ((self depth-first-strategy))
  (initialize-queue self))

(defmethod list-activations ((self depth-first-strategy))
  (get-all-activations (get-priority-queue self)))

(defmethod initialize-queue ((self depth-first-strategy))
  (setf (slot-value self 'priority-queue)
    (make-indexed-priority-list
     #'(lambda (obj place) (push obj place)))))

(defmethod initialize-instance :after ((self depth-first-strategy) &rest args)
  (initialize-queue self))

(defun make-depth-first-strategy ()
  (make-instance 'depth-first-strategy))

