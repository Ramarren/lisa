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

;;; $Id: rete.lisp,v 1.22 2000/11/30 20:00:26 youngde Exp $

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
   (initial-fact :initform 
                 (make-fact (find-class 'initial-fact) nil)
                 :reader get-initial-fact)
   (clear-fact :initform
               (make-fact (find-class 'clear-fact) nil)
               :reader get-clear-fact)
   (null-fact :initform
              (make-fact (find-class 'not-or-test-fact) nil)
              :reader get-null-fact)
   (fact-list :initform (make-hash-table)
              :accessor get-facts)
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

(defmethod destroy-activation ((self rete) token)
  (remove-activation (get-strategy self) token))

(defmethod increment-time ((self rete))
  (incf (get-clock self)))

(defmethod get-engine-time ((self rete))
  (get-clock self))

(defmethod record-fact ((self rete) fact)
  (with-accessors ((facts get-facts)) self
    (setf (gethash (get-fact-id fact) facts) fact)))

#+ignore
(defmethod find-fact ((self rete) (id integer))
  (find-if #'(lambda (fact)
               (= id (get-fact-id fact)))
           (get-facts self)))

#+ignore
(defmethod remove-fact ((self rete) (f fact))
  (let ((id (get-fact-id f)))
    (with-accessors ((facts get-facts)) self
      (setf facts
        (delete-if #'(lambda (fact)
                       (= id (get-fact-id fact)))
                   facts)))))

#+ignore
(defmethod remove-facts ((self rete))
  (setf (get-facts self) nil))

(defmethod find-fact ((self rete) (id integer))
  (gethash id (get-facts self)))

(defmethod remove-fact ((self rete) fact)
  (remhash (get-fact-id fact) (get-facts self)))

(defmethod remove-facts ((self rete))
  (clrhash (get-facts self)))

(defmethod get-fact-list ((self rete))
  (flet ((retrieve-value (key val)
           (declare (ignore key))
           (values val)))
    (sort (hash-to-list #'retrieve-value (get-facts self))
          #'(lambda (f1 f2)
              (< (get-fact-id f1) (get-fact-id f2))))))

(defmethod next-fact-id ((self rete))
  (with-accessors ((next-fact-id get-next-fact-id)) self
    (prog1
        (values next-fact-id)
      (incf next-fact-id))))

(defmethod insert-token ((self rete) token)
  (call-node-right (get-root-node (get-compiler self)) token))

(defmethod assert-fact ((self rete) fact)
  (setf (get-fact-id fact) (next-fact-id self))
  (increment-time self)
  (update-time fact self)
  (record-fact self fact)
  (insert-token self (make-add-token :initial-fact fact))
  (values fact))

(defmethod retract-fact ((self rete) (fact fact))
  (remove-fact self fact)
  (increment-time self)
  (update-time fact self)
  (insert-token self (make-remove-token :initial-fact fact))
  (values fact))
  
(defmethod retract-fact ((self rete) (fact-id integer))
  (let ((fact (find-fact self fact-id)))
    (unless (null fact)
      (retract-fact fact))
    (values fact)))
    
(defmethod set-initial-state ((self rete))
  (remove-facts self)
  (remove-activations (get-strategy self))
  (setf (get-next-fact-id self) 0)
  (setf (get-clock self) 0)
  (values t))

(defmethod reset-engine ((self rete))
  (insert-token self (make-clear-token
                      :initial-fact (get-clear-fact self)))
  (set-initial-state self)
  (assert-fact self (get-initial-fact self))
  (values t))

(defmethod clear-engine ((self rete))
  (set-initial-state self)
  (setf (slot-value self 'compiler) (make-rete-compiler))
  (values t))

(defmethod get-rule-list ((self rete))
  (let ((rules (list)))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (push val rules))
             (get-rules self))
    (values rules)))

(defmethod get-activation-list ((self rete))
  (get-activations (get-strategy self)))

(defmethod run-engine ((self rete))
  (let ((strategy (get-strategy self)))
    (do ((activation (next-activation strategy)
                     (next-activation strategy)))
        ((null activation) t)
      (fire-rule activation))))

(defun make-rete (&key (strategy (make-depth-first-strategy)))
  (make-instance 'rete :strategy strategy))
