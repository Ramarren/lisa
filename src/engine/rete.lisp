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

;;; $Id: rete.lisp,v 1.57 2001/05/09 20:12:56 youngde Exp $

(in-package "LISA")

(defclass rete (synchronizable)
  ((rules :initform (make-hash-table)
          :accessor get-rules)
   (strategy :initarg :strategy
             :initform nil
             :reader get-strategy)
   (compiler :initform (make-rete-compiler)
             :reader get-compiler)
   (clock :initform 0
          :accessor get-clock)
   (initial-fact :initform (make-initial-fact)
                 :reader get-initial-fact)
   (clear-fact :initform (make-clear-fact)
               :reader get-clear-fact)
   (null-fact :initform (make-not-or-test-fact)
              :reader get-null-fact)
   (fact-list :initform (make-hash-table)
              :accessor get-facts)
   (next-fact-id :initform 0
                 :accessor get-next-fact-id)
   (halted-p :initform nil)
   (fired-rule-count :initform 0
                     :reader get-fired-rule-count))
  (:documentation
   "Represents the inference engine itself."))

(defun add-rule (self rule)
  (declare (type rete self))
  (with-accessors ((rules get-rules)) self
    (add-rule-to-network (get-compiler self) rule)
    (setf (gethash (get-name rule) rules) rule)))

(defun remove-rules (self)
  (declare (type rete self))
  (clrhash (get-rules self)))

(defun engine-halted-p (self)
  (declare (type rete self))
  (slot-value self 'halted-p))

(defmethod add-activation ((self rete) activation)
  (watchpoint 'enable-activation activation)
  (add-activation (get-strategy self) activation))

(defmethod disable-activation ((self rete) activation)
  (when (eligible-p activation)
    (watchpoint 'disable-activation activation)
    (setf (get-eligible activation) nil)))

(defmethod find-activation ((self rete) rule token)
  (find-activation (get-strategy self) rule token))

(defun increment-time (self)
  (declare (type rete self))
  (incf (get-clock self)))

(defun get-engine-time (self)
  (declare (type rete self))
  (get-clock self))

(defun record-fact (self fact)
  (declare (type rete self) (type fact fact))
  (with-accessors ((facts get-facts)) self
    (setf (gethash (get-fact-id fact) facts) fact)))

(defun lookup-fact (self id)
  (declare (type rete self) (type integer id))
  (gethash id (get-facts self)))

(defun remove-fact (self fact)
  (declare (type rete self) (type fact fact))
  (remhash (get-fact-id fact) (get-facts self)))

(defun remove-facts (self)
  (declare (type rete self))
  (clrhash (get-facts self)))

(defun get-fact-list (self)
  (declare (type rete self))
  (flet ((retrieve-value (key val)
           (declare (ignore key))
           (values val)))
    (sort (lsthash #'retrieve-value (get-facts self))
          #'(lambda (f1 f2)
              (< (get-fact-id f1) (get-fact-id f2))))))

#+ignore ; What do we do about shadow-facts?
(defun save-facts (self strm)
  (declare (type rete self))
  (mapc #'(lambda (fact)
            (write-fact fact strm))
        (get-fact-list self))
  (values))

(defun next-fact-id (self)
  (declare (type rete self))
  (with-accessors ((next-fact-id get-next-fact-id)) self
    (prog1
        (values next-fact-id)
      (incf next-fact-id))))

(defun insert-token (self token)
  (declare (type rete self) (type token token))
  (update-time (get-top-fact token) self)
  (call-node-right (get-root-node (get-compiler self)) token))

(defmethod assert-fact ((self rete) (fact fact))
  (with-synchronization (self)
    (set-fact-id fact (next-fact-id self))
    (increment-time self)
    (record-fact self fact)
    (watchpoint 'assert fact)
    (insert-token self (make-add-token :initial-fact fact))
    (values fact)))

(defmethod assert-fact ((self rete) (fact shadow-fact))
  (bind-clos-instance self (instance-of-shadow-fact fact) fact)
  (call-next-method self fact))

(defmethod retract-fact ((self rete) (fact fact))
  (with-synchronization (self)
    (remove-fact self fact)
    (increment-time self)
    (watchpoint 'retract fact)
    (insert-token self (make-remove-token :initial-fact fact))
    (values fact)))
  
(defmethod retract-fact ((self rete) (fact shadow-fact))
  (unbind-clos-instance self (instance-of-shadow-fact fact))
  (call-next-method self fact))
  
(defmethod retract-fact ((self rete) (fact-id integer))
  (with-synchronization (self)
    (let ((fact (lookup-fact self fact-id)))
      (unless (null fact)
        (retract-fact self fact))
      (values fact))))

(defmethod modify-fact ((self rete) fact slot-changes)
  (with-synchronization (self)
    (insert-token self (make-remove-token :initial-fact fact))
    (mapc #'(lambda (slot)
              (set-slot-value fact (first slot) (second slot)))
          slot-changes)
    (insert-token self (make-add-token :initial-fact fact))
    (values fact)))

(defmethod mark-clos-instance-as-changed ((self rete) instance
                                          &optional (slot-id nil))
  (with-synchronization (self)
    (let ((shadow-fact (find-shadow-fact self instance)))
      (cond ((null shadow-fact)
             (warn "This instance is not known to LISA: ~S." instance))
            (t
             (insert-token self (make-remove-token :initial-fact shadow-fact))
             (synchronize-with-instance shadow-fact slot-id)
             (insert-token self (make-add-token :initial-fact shadow-fact))))
      (values instance))))

(defun set-initial-state (self)
  (declare (type rete self))
  (remove-facts self)
  (remove-activations (get-strategy self))
  (setf (get-next-fact-id self) 0)
  (setf (get-clock self) 0)
  (setf (slot-value self 'fired-rule-count) 0)
  (values t))

(defun reset-engine (self)
  (declare (type rete self))
  (insert-token self (make-clear-token
                      :initial-fact (get-clear-fact self)))
  (set-initial-state self)
  (assert-fact self (get-initial-fact self))
  (values t))

(defun forget-clos-instances (self)
  (declare (type rete self))
  (maphash #'(lambda (key fact)
               (declare (ignore key))
               (when (typep fact 'shadow-fact)
                 (unbind-clos-instance
                  self (instance-of-shadow-fact fact))))
           (get-facts self)))
  
(defun clear-engine (self)
  (declare (type rete self))
  (forget-clos-instances self)
  (set-initial-state self)
  (remove-rules self)
  (setf (slot-value self 'compiler) (make-rete-compiler))
  (values t))

(defun get-rule-list (self)
  (declare (type rete self))
  (let ((rules (list)))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (push val rules))
             (get-rules self))
    (values rules)))

(defun get-activation-list (self)
  (declare (type rete self))
  (list-activations (get-strategy self)))

(defmethod run-engine ((self rete) &optional (step t))
  (flet ((prepare-for-run ()
           (setf (slot-value self 'halted-p) nil)))
    (let ((strategy (get-strategy self)))
      (prepare-for-run)
      (do ((count 0))
          ((or (eql count step) (engine-halted-p self)) count)
        (with-synchronization (self)
          (let ((activation (next-activation strategy)))
            (cond ((null activation)
                   (halt-engine self))
                  ((eligible-p activation)
                   (incf (slot-value self 'fired-rule-count))
                   (fire-rule activation)
                   (incf count)))))
        (lmp:process-yield)))))

(defun halt-engine (self)
  (declare (type rete self))
  (setf (slot-value self 'halted-p) t))

(defun make-rete (strategy)
  (make-instance 'rete :strategy strategy))
