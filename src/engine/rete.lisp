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

;;; File: rete.lisp
;;; Description: Class representing the inference engine itself.

;;; $Id: rete.lisp,v 1.47 2001/03/15 20:53:29 youngde Exp $

(in-package "LISA")

(defclass rete ()
  ((rules :initform (make-hash-table)
          :accessor get-rules)
   (strategy :initarg :strategy
             :initform nil
             :reader get-strategy)
   (compiler :initform (make-rete-compiler)
             :reader get-compiler)
   (clock :initform 0
          :accessor get-clock)
   (initial-fact :initform 
                 (make-fact (class-name (find-class 'initial-fact)) nil)
                 :reader get-initial-fact)
   (clear-fact :initform
               (make-fact (class-name (find-class 'clear-fact)) nil)
               :reader get-clear-fact)
   (null-fact :initform
              (make-fact (class-name (find-class 'not-or-test-fact)) nil)
              :reader get-null-fact)
   (fact-list :initform (make-hash-table)
              :accessor get-facts)
   (next-fact-id :initform 0
                 :accessor get-next-fact-id)
   (fired-rule-count :initform 0
                     :reader get-fired-rule-count))
  (:documentation
   "Represents the inference engine itself."))

(defmethod add-rule ((self rete) rule)
  (with-accessors ((rules get-rules)) self
    (add-rule-to-network (get-compiler self) rule)
    (setf (gethash (get-name rule) rules) rule)))

(defmethod remove-rules ((self rete))
  (clrhash (get-rules self)))

(defmethod add-activation ((self rete) activation)
  (watchpoint 'enable-activation activation)
  (add-activation (get-strategy self) activation))

(defmethod disable-activation ((self rete) activation)
  (when (eligible-p activation)
    (watchpoint 'disable-activation activation)
    (setf (get-eligible activation) nil)))

(defmethod find-activation ((self rete) rule token)
  (find-activation (get-strategy self) rule token))

(defmethod increment-time ((self rete))
  (incf (get-clock self)))

(defmethod get-engine-time ((self rete))
  (get-clock self))

(defmethod record-fact ((self rete) fact)
  (with-accessors ((facts get-facts)) self
    (setf (gethash (get-fact-id fact) facts) fact)))

(defmethod lookup-fact ((self rete) (id integer))
  (gethash id (get-facts self)))

(defmethod remove-fact ((self rete) fact)
  (remhash (get-fact-id fact) (get-facts self)))

(defmethod remove-facts ((self rete))
  (clrhash (get-facts self)))

(defmethod get-fact-list ((self rete))
  (flet ((retrieve-value (key val)
           (declare (ignore key))
           (values val)))
    (sort (lsthash #'retrieve-value (get-facts self))
          #'(lambda (f1 f2)
              (< (get-fact-id f1) (get-fact-id f2))))))

(defmethod save-facts ((self rete) strm)
  (mapc #'(lambda (fact)
            (write-fact fact strm))
        (get-fact-list self))
  (values))

(defmethod next-fact-id ((self rete))
  (with-accessors ((next-fact-id get-next-fact-id)) self
    (prog1
        (values next-fact-id)
      (incf next-fact-id))))

(defmethod insert-token ((self rete) token)
  (update-time (get-top-fact token) self)
  (call-node-right (get-root-node (get-compiler self)) token))

(defmethod assert-fact ((self rete) fact)
  (set-fact-id fact (next-fact-id self))
  (increment-time self)
  (record-fact self fact)
  (watchpoint 'assert fact)
  (insert-token self (make-add-token :initial-fact fact))
  (values fact))

(defmethod retract-fact ((self rete) (fact fact))
  (remove-fact self fact)
  (increment-time self)
  (watchpoint 'retract fact)
  (insert-token self (make-remove-token :initial-fact fact))
  (values fact))
  
(defmethod retract-fact ((self rete) (fact-id integer))
  (let ((fact (lookup-fact self fact-id)))
    (unless (null fact)
      (retract-fact self fact))
    (values fact)))

(defmethod modify-fact ((self rete) fact slot-changes)
  (insert-token self (make-remove-token :initial-fact fact))
  (mapc #'(lambda (slot)
            (set-slot-value fact (first slot) (second slot)))
        slot-changes)
  (insert-token self (make-add-token :initial-fact fact))
  (values fact))
  
(defmethod set-initial-state ((self rete))
  (remove-facts self)
  (remove-activations (get-strategy self))
  (setf (get-next-fact-id self) 0)
  (setf (get-clock self) 0)
  (setf (slot-value self 'fired-rule-count) 0)
  (values t))

(defmethod reset-engine ((self rete))
  (insert-token self (make-clear-token
                      :initial-fact (get-clear-fact self)))
  (set-initial-state self)
  (assert-fact self (get-initial-fact self))
  (values t))

(defmethod clear-engine ((self rete))
  (set-initial-state self)
  (remove-rules self)
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
  (list-activations (get-strategy self)))

(defun run-engine (self &optional (step t))
  (let ((strategy (get-strategy self))
        (count 0))
    (loop
      (if (eql count step)
          (return count)
        (let ((activation (next-activation strategy)))
          (cond ((null activation)
                 (return count))
                ((eligible-p activation)
                 (incf (slot-value self 'fired-rule-count))
                 (fire-rule activation)
                 (incf count))))))))

(defun make-rete (&key (strategy (make-breadth-first-strategy)))
  (make-instance 'rete :strategy strategy))
