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

;;; File: rete.lisp
;;; Description: Class representing the inference engine itself.

;;; $Id: rete.lisp,v 1.11 2000/11/17 16:45:51 youngde Exp $

(in-package :lisa)

(defclass rete ()
  ((hash-index :initarg :hash-index
               :initform 0
               :accessor get-hash-index)
   (rules :initform (make-hash-table)
          :accessor get-rules)
   (strategy :initarg :strategy
             :initform nil
             :reader get-strategy)
   (compiler :initform (make-rete-compiler)
             :reader get-compiler)
   (clock :initform 0
          :accessor get-clock)
   (fact-list :initform nil
              :accessor get-fact-list)
   (next-fact-id :initform 0
                 :accessor get-next-fact-id))
  (:documentation
   "Represents the inference engine itself."))

(defmethod add-rule ((self rete) rule)
  (with-accessors ((rules get-rules)) self
    (add-rule-to-network (get-compiler self) rule)
    (setf (gethash (get-name rule) rules) rule)))

(defmethod create-activation ((self rete) rule token)
  (add-activation (get-strategy self)
                  (make-activation rule token)))

(defmethod increment-time ((self rete))
  (incf (get-clock self)))

(defmethod record-fact ((self rete) fact)
  (with-accessors ((facts get-fact-list)) self
    (setf facts
      (nconc facts `(,fact)))))

(defmethod next-fact-id ((self rete))
  (with-accessors ((next-fact-id get-next-fact-id)) self
    (prog1
        (values next-fact-id)
      (incf next-fact-id))))

(defmethod insert-token ((self rete) (token add-token))
  (call-node-right (get-root-node (get-compiler self)) token))

(defmethod assert-fact ((self rete) fact)
  (setf (get-fact-id fact) (next-fact-id self))
  (increment-time self)
  (update-time fact self)
  (record-fact self fact)
  (insert-token self (make-add-token :initial-fact fact))
  (values fact))
  
(defmethod run ((self rete))
  (mapc #'(lambda (activation)
            (fire-rule activation))
        (get-activations (get-strategy self))))

(defun make-rete (&key (strategy (make-depth-first-strategy)))
  (make-instance 'rete :strategy strategy))
