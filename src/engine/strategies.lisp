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

;;; $Id: strategies.lisp,v 1.21 2001/03/05 16:35:18 youngde Exp $

(in-package :lisa)

(defclass strategy ()
  ((activations :initarg :activations
                :initform nil
                :accessor get-activations))
  (:documentation
   "Serves as the base class for all classes implementing conflict
   resolution strategies."))

(defgeneric add-activation (strategy activation))
(defgeneric find-activation (strategy rule token))
(defgeneric next-activation (strategy))
(defgeneric remove-activations (strategy))
(defgeneric list-activations (strategy))

(defclass indexed-priority-list ()
  ((priority-vector :reader get-priority-vector)
   (inodes :initform nil
           :accessor get-inodes)
   (delta :accessor get-delta)
   (insertion-function :initarg :insertion-function
                       :reader get-insertion-function))
  (:documentation
   "Utility class that implements an indexed priority 'queue' to manage
   activations. Employed by various types of conflict resolution strategies,
   particularly DEPTH-FIRST-STRATEGY and BREADTH-FIRST-STRATEGY."))

(defmethod initialize-instance :after ((self indexed-priority-list)
                                       &key (priorities 500)
                                       (insertion-function nil))
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
               `(,activation ,queue)))))))

(defun lookup-activation (plist rule token)
  (find-if #'(lambda (act)
               (and (= (hash-code act) (hash-code token))
                    (eq (get-rule act) rule)))
           (aref (get-priority-vector plist)
                 (+ (get-salience rule) (get-delta plist)))))

(defun get-next-activation (plist)
  (declare (type indexed-priority-list plist))
  (with-accessors ((inodes get-inodes)
                   (vector get-priority-vector)) plist
    (let ((inode (first inodes)))
      (cond ((null inode)
             (values nil))
            (t
             (let ((activation (pop (aref vector inode))))
               (when (null (aref vector inode))
                 (pop inodes))
               (values activation)))))))

(defun get-all-activations (plist)
  (let ((activations (list)))
    (with-accessors ((queue get-priority-vector)) plist
      (mapc #'(lambda (inode)
                (mapc #'(lambda (act)
                          (when (eligible-p act)
                            (push act activations)))
                      (aref queue inode)))
            (get-inodes plist)))
    (nreverse activations)))

(defun make-indexed-priority-list (func)
  (make-instance 'indexed-priority-list :insertion-function func))

(defclass depth-first-strategy (strategy)
  ((priority-queue :reader get-priority-queue))
  (:documentation
   "A depth-first conflict resolution strategy."))

(defmethod add-activation ((self depth-first-strategy) activation)
  (insert-activation (get-priority-queue self) activation))

(defmethod find-activation ((self depth-first-strategy) rule token)
  (lookup-activation (get-priority-queue self) rule token))

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
  (declare (ignore args))
  (initialize-queue self))

(defun make-depth-first-strategy ()
  (make-instance 'depth-first-strategy))

(defclass breadth-first-strategy (strategy)
  ((priority-queue :reader get-priority-queue))
  (:documentation
   "A breadth-first conflict resolution strategy."))

(defmethod add-activation ((self breadth-first-strategy) activation)
  (insert-activation (get-priority-queue self) activation))

(defmethod find-activation ((self breadth-first-strategy) rule token)
  (lookup-activation (get-priority-queue self) rule token))

(defmethod next-activation ((self breadth-first-strategy))
  (get-next-activation (get-priority-queue self)))

(defmethod remove-activations ((self breadth-first-strategy))
  (initialize-queue self))

(defmethod list-activations ((self breadth-first-strategy))
  (get-all-activations (get-priority-queue self)))

(defmethod initialize-queue ((self breadth-first-strategy))
  (setf (slot-value self 'priority-queue)
    (make-indexed-priority-list
     #'(lambda (obj place) (append place `(,obj))))))

(defmethod initialize-instance :after ((self breadth-first-strategy) &rest args)
  (declare (ignore args))
  (initialize-queue self))

(defun make-breadth-first-strategy ()
  (make-instance 'breadth-first-strategy))

