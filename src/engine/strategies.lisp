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

;;; File: strategies.lisp
;;; Description: Classes that implement the various default conflict
;;; resolution strategies for Lisa's RETE implementation.

;;; $Id: strategies.lisp,v 1.27 2001/08/30 17:37:12 youngde Exp $

(in-package "LISA")

(defclass strategy ()
  ()
  (:documentation
   "Serves as the base class for all classes implementing conflict
   resolution strategies."))

(defgeneric add-activation (strategy activation))
(defgeneric find-activation (strategy rule token))
(defgeneric find-activations (strategy rule))
(defgeneric next-activation (strategy))
(defgeneric remove-activations (strategy))
(defgeneric list-activations (strategy))

(defclass indexed-priority-list ()
  ((priority-vector :reader get-priority-vector)
   (inodes :initform '()
           :accessor get-inodes)
   (delta :accessor get-delta)
   (insertion-function :initarg :insertion-function
                       :reader get-insertion-function))
  (:documentation
   "Utility class that implements an indexed priority 'queue' to manage
   activations. Employed by various types of conflict resolution strategies,
   particularly DEPTH-FIRST-STRATEGY and BREADTH-FIRST-STRATEGY."))

(defmethod initialize-instance :after ((self indexed-priority-list)
                                       &key (priorities 500))
  (setf (slot-value self 'priority-vector)
    (make-array (1+ priorities) :initial-element nil))
  (setf (slot-value self 'delta) (/ priorities 2)))

(defun reset-activations (self)
  (declare (type indexed-priority-list self))
  (let ((queue (get-priority-vector self)))
    (mapc #'(lambda (inode)
              (setf (aref queue inode) nil))
          (get-inodes self))
    (setf (get-inodes self) '())))

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

(defun lookup-activation (self rule token)
  (declare (type indexed-priority-list self))
  (find-if #'(lambda (act)
               (and (= (hash-code act) (hash-code token))
                    (eq (get-rule act) rule)))
           (aref (get-priority-vector self)
                 (+ (get-salience rule) (get-delta self)))))

(defun lookup-activations (self rule)
  (declare (type indexed-priority-list self))
  (let ((rule-name (get-name rule)))
    (collect #'(lambda (activation)
                 (eql rule-name (get-name (get-rule activation))))
             (aref (get-priority-vector self)
                   (+ (get-salience rule) (get-delta self))))))

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

(defclass builtin-strategy (strategy)
  ((priority-queue :reader get-priority-queue))
  (:documentation
   "A base class for all LISA builtin conflict resolution strategies."))
  
(defmethod add-activation ((self builtin-strategy) activation)
  (insert-activation (get-priority-queue self) activation))

(defmethod find-activation ((self builtin-strategy) rule token)
  (lookup-activation (get-priority-queue self) rule token))

(defmethod find-activations ((self builtin-strategy) rule)
  (lookup-activations (get-priority-queue self) rule))

(defmethod next-activation ((self builtin-strategy))
  (get-next-activation (get-priority-queue self)))

(defmethod remove-activations ((self builtin-strategy))
  (reset-activations (get-priority-queue self)))

(defmethod list-activations ((self builtin-strategy))
  (get-all-activations (get-priority-queue self)))

(defclass depth-first-strategy (builtin-strategy)
  ()
  (:documentation
   "A depth-first conflict resolution strategy."))

(defmethod initialize-instance :after ((self depth-first-strategy) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'priority-queue)
    (make-indexed-priority-list
     #'(lambda (obj place) (push obj place)))))

(defun make-depth-first-strategy ()
  (make-instance 'depth-first-strategy))

(defclass breadth-first-strategy (builtin-strategy)
  ()
  (:documentation
   "A breadth-first conflict resolution strategy."))

(defmethod initialize-instance :after ((self breadth-first-strategy) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'priority-queue)
    (make-indexed-priority-list
     #'(lambda (obj place) (nconc place `(,obj))))))

(defun make-breadth-first-strategy ()
  (make-instance 'breadth-first-strategy))

