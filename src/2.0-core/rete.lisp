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

;;; $Id: rete.lisp,v 1.35 2002/11/15 16:36:05 youngde Exp $

(in-package "LISA")

(defclass rete ()
  ((rule-table :initform (make-hash-table)
               :reader rete-rule-table)
   (fact-table :initform (make-hash-table)
               :reader rete-fact-table)
   (instance-table :initform (make-hash-table)
                   :reader rete-instance-table)
   (rete-network :initform (make-rete-network)
                 :reader rete-network)
   (next-fact-id :initform -1
                 :accessor rete-next-fact-id)
   (autofacts :initform (list)
              :accessor rete-autofacts)
   (meta-data :initform (make-hash-table)
              :reader rete-meta-data)
   (dependency-table :initform (make-hash-table :test #'equal)
                     :accessor rete-dependency-table)
   (halted :initform nil
           :accessor rete-halted)
   (firing-count :initform 0
                 :accessor rete-firing-count)
   (strategy :initarg :strategy
             :initform nil
             :reader rete-strategy)))

;;; FACT-META-OBJECT represents data about facts. Every LISA fact is backed by
;;; a CLOS instance that was either defined by the application or internally
;;; by LISA (via DEFTEMPLATE). FACT-META-OBJECT performs housekeeping chores;
;;; mapping symbolic fact names to actual class names, slot names to their
;;; corresponding effective slot names, etc.

#+ignore
(defstruct fact-meta-object
  (symbolic-name nil :type symbol)
  (class-name nil :type symbol)
  (slot-table (make-hash-table))
  (superclasses nil :type list))

(defstruct fact-meta-object
  (class-name nil :type symbol)
  (slot-list nil :type list)
  (superclasses nil :type list))

(defun register-meta-object (rete key meta-object)
  (setf (gethash key (rete-meta-data rete)) meta-object))

(defun find-meta-object (rete symbolic-name)
  (gethash symbolic-name (rete-meta-data rete)))

(defun rete-fact-count (rete)
  (hash-table-count (rete-fact-table rete)))

(defun find-rule (rete rule-name)
  (values (gethash rule-name (rete-rule-table rete))))

(defun add-rule-to-network (rete rule patterns)
  (flet ((load-facts (network)
           (loop for fact being the hash-value
               of (rete-fact-table rete)
               do (add-fact-to-network network fact))))
    (when (find-rule rete (rule-name rule))
      (forget-rule rete rule))
    (if (zerop (rete-fact-count rete))
        (compile-rule-into-network (rete-network rete) patterns rule)
      (merge-rule-into-network 
       (rete-network rete) patterns rule :loader #'load-facts))
    (setf (gethash (rule-name rule) (rete-rule-table rete)) rule)
    rule))

(defmethod forget-rule ((self rete) (rule-name symbol))
  (macrolet ((disable-activations (rete rule)
               `(mapc #'(lambda (activation)
                          (setf (activation-eligible activation) nil))
                      (find-all-activations
                       (rete-strategy ,rete) ,rule))))
    (let ((rule (find-rule self rule-name)))
      (cl:assert (not (null rule)) nil
        "The rule named ~S is not known to be defined." rule-name)
      (remove-rule-from-network (rete-network self) rule)
      (remhash rule-name (rete-rule-table self))
      (disable-activations self rule)
      (when (composite-rule-p rule)
        (dolist (subrule (rule-subrules rule))
          (forget-rule self subrule)))
      rule)))

(defmethod forget-rule ((self rete) (rule rule))
  (forget-rule self (rule-name rule)))

(defun remember-fact (rete fact)
  (setf (gethash (fact-id fact) (rete-fact-table rete)) fact))

(defun forget-fact (rete fact)
  (remhash (fact-id fact) (rete-fact-table rete)))

(defun find-fact-by-id (rete fact-id)
  (gethash fact-id (rete-fact-table rete)))

(defun forget-all-facts (rete)
  (clrhash (rete-fact-table rete)))

(defun make-fact-list (rete)
  (sort
   (loop for fact being the hash-value of (rete-fact-table rete)
       collect fact)
   #'(lambda (f1 f2) (< (fact-id f1) (fact-id f2)))))

(defun next-fact-id (rete)
  (incf (rete-next-fact-id rete)))

(defun add-autofact (rete deffact)
  (pushnew deffact (rete-autofacts rete) :key #'deffacts-name))

(defun remove-autofacts (rete)
  (setf (rete-autofacts rete) nil))

(defun assert-autofacts (rete)
  (mapc #'(lambda (deffact)
            (mapc #'(lambda (fact)
                      (assert-fact rete (make-fact-from-template fact)))
                  (deffacts-fact-list deffact)))
        (rete-autofacts rete)))

(defmethod assert-fact ((self rete) fact)
  (with-truth-maintenance (self)
    (setf (fact-id fact) (next-fact-id self))
    (remember-fact self fact)
    (trace-assert fact)
    (add-fact-to-network (rete-network self) fact)
    fact))

(defmethod retract-fact ((self rete) (fact fact))
  (with-truth-maintenance (self)
    (forget-fact self fact)
    (trace-retract fact)
    (remove-fact-from-network (rete-network self) fact)
    fact))

(defmethod retract-fact ((self rete) (fact-id integer))
  (let ((fact (find-fact-by-id self fact-id)))
    (and (not (null fact))
         (retract-fact self fact))))

(defmethod modify-fact ((self rete) fact &rest slot-changes)
  (retract-fact self fact)
  (mapc #'(lambda (slot)
            (set-slot-value fact (first slot) (second slot)))
        slot-changes)
  (assert-fact self fact)
  fact)

(defun set-initial-state (rete)
  (forget-all-facts rete)
  (remove-activations (rete-strategy rete))
  (setf (rete-next-fact-id rete) -1)
  (setf (rete-firing-count rete) 0)
  t)

(defmethod reset-engine ((self rete))
  (reset-network (rete-network self))
  (set-initial-state self)
  (assert (initial-fact))
  (assert-autofacts self)
  t)

(defun make-rule-list (rete)
  (loop for rule being the hash-value of (rete-rule-table rete)
      collect rule))

(defun make-activation-list (rete)
  (list-activations (rete-strategy rete)))

(defun find-fact-using-instance (rete instance)
  (gethash instance (rete-instance-table rete)))

(defun forget-clos-instances (rete)
  (clrhash (rete-instance-table rete)))

(defmethod mark-clos-instance-as-changed ((self rete) instance
                                          &optional (slot-id nil))
  (let ((fact (find-fact-using-instance self instance))
        (network (rete-network self)))
    (cond ((null fact)
           (warn "This instance is not known to LISA: ~S." instance))
          (t
           (remove-fact-from-network network fact)
           (synchronize-with-instance fact slot-id)
           (add-fact-to-network network fact)))
    instance))

(defmethod add-activation ((self rete) activation)
  (trace-enable-activation activation)
  (add-activation (rete-strategy self) activation))

(defmethod disable-activation ((self rete) activation)
  (when (eligible-p activation)
    (trace-disable-activation activation)
    (setf (activation-eligible activation) nil)))

(defmethod run-engine ((self rete) &optional (step -1))
  (let ((strategy (rete-strategy self)))
    (setf (rete-halted self) nil)
    (do ((count 0))
        ((or (= count step) (rete-halted self)) count)
      (let ((activation (next-activation strategy)))
        (cond ((null activation)
               (halt-engine self))
              ((eligible-p activation)
               (incf (rete-firing-count self))
               (fire-activation activation)
               (incf count)))))))

(defun halt-engine (rete)
  (setf (rete-halted rete) t))

(defun make-rete (&optional (strategy nil))
  (make-instance 'rete 
    :strategy (if strategy strategy (make-breadth-first-strategy))))

(defun make-inference-engine ()
  (make-rete))

(defun make-query-engine (source-rete)
  (let* ((query-engine (make-inference-engine)))
    (loop for fact being the hash-value 
        of (rete-fact-table source-rete)
        do (remember-fact query-engine fact))
    query-engine))
