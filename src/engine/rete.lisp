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

;;; $Id: rete.lisp,v 1.67 2002/05/31 02:51:21 youngde Exp $

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
   (initial-fact :initform nil
                 :reader get-initial-fact)
   (clear-fact :initform nil
               :reader get-clear-fact)
   (null-fact :initform nil
              :reader get-null-fact)
   (autofacts :initform '()
              :accessor get-autofacts)
   (fact-list :initform (make-hash-table)
              :accessor get-facts)
   (instance-table :initform (make-hash-table)
                   :accessor get-instance-table)
   (meta-data :initform (make-meta-data)
              :reader get-meta-data)
   (next-fact-id :initform 0
                 :accessor get-next-fact-id)
   (halted-p :initform nil)
   (fired-rule-count :initform 0
                     :reader get-fired-rule-count))
  (:documentation
   "Represents the inference engine itself."))

(defmethod initialize-instance :after ((self rete) &rest args)
  (with-inference-engine (self)
    (deftemplate initial-fact ())
    (deftemplate clear-fact ())
    (deftemplate not-or-test-fact ())
    (deftemplate query-fact () (slot lisa::query-name))
    (setf (slot-value self 'initial-fact)
      (make-fact 'initial-fact '()))
    (setf (slot-value self 'clear-fact)
      (make-fact 'clear-fact '()))
    (setf (slot-value self 'null-fact)
      (make-fact 'not-or-test-fact '()))
    self))

(defun meta-fact-map (rete)
  (meta-data-fact-map (get-meta-data rete)))

(defun meta-class-map (rete)
  (meta-data-class-map (get-meta-data rete)))

(defun fact-count (rete)
  (hash-table-count (get-facts rete)))

(defun find-rule (rete rule-name)
  (gethash rule-name (get-rules rete)))

(defun remove-rule (rete rule)
  (let ((rule-name (get-name rule)))
    (remove-rule-from-network rete rule-name)
    (remhash rule-name (get-rules rete))
    (mapc #'(lambda (activation)
              (disable-activation rete activation))
          (find-activations (get-strategy rete) rule))
    rule))

(defun remove-rules (rete)
  (clrhash (get-rules rete)))

(defmethod undefine-rule ((self rete) (rule-instance rule))
  (remove-rule self rule-instance))

(defmethod undefine-rule ((self rete) (rule-name symbol))
  (let ((instance (find-rule self rule-name)))
    (if (not (null instance))
        (remove-rule self instance)
      (warn "There's no rule in the network named ~S." rule-name))))
  
(defun synchronize-rule (rete rule)
  (with-dynamic-update (rete rule)
    (mapc #'(lambda (fact)
              (insert-token 
               rete (make-add-token :initial-fact fact)))
          (get-fact-list rete))))
  
(defun add-rule (rete rule)
  (with-accessors ((rules get-rules)) rete
    (let ((rule-name (get-name rule)))
      (unless (null (find-rule rete rule-name))
        (remove-rule rete rule))
      (add-rule-to-network (get-compiler rete) rule)
      (setf (gethash rule-name rules) rule)
      (when (plusp (fact-count rete))
        (synchronize-rule rete rule))
      rule)))

;;; This function is a bit brutal in terms of efficiency, but I don't expect a
;;; large number of DEFFACTs per inference engine...

(defun add-autofact (rete deffact)
  (with-accessors ((autofacts get-autofacts)) rete
    (setf autofacts
      (delete-if #'(lambda (fact)
                     (eql (get-name fact) (get-name deffact)))
                 autofacts))
    (setf autofacts (nconc autofacts `(,deffact))))
  (values))

(defun remove-autofacts (rete)
  (setf (get-autofacts rete) nil))

(defun engine-halted-p (rete)
  (slot-value rete 'halted-p))

(defmethod add-activation ((self rete) activation)
  (watchpoint 'enable-activation activation)
  (add-activation (get-strategy self) activation))

(defmethod disable-activation ((self rete) activation)
  (when (eligible-p activation)
    (watchpoint 'disable-activation activation)
    (setf (get-eligible activation) nil)))

(defmethod find-activation ((self rete) rule token)
  (find-activation (get-strategy self) rule token))

(defun increment-time (rete)
  (incf (get-clock rete)))

(defun get-engine-time (rete)
  (get-clock rete))

(defun record-fact (rete fact)
  (with-accessors ((facts get-facts)) rete
    (setf (gethash (get-fact-id fact) facts) fact)))

(defun lookup-fact (rete id)
  (declare (type integer id))
  (gethash id (get-facts rete)))

(defun remove-fact (rete fact)
  (remhash (get-fact-id fact) (get-facts rete)))

(defun remove-facts (rete)
  (clrhash (get-facts rete)))

(defun get-fact-list (rete)
  (flet ((retrieve-value (key val)
           (declare (ignore key))
           (values val)))
    (sort (lsthash #'retrieve-value (get-facts rete))
          #'(lambda (f1 f2)
              (< (get-fact-id f1) (get-fact-id f2))))))

(defun next-fact-id (rete)
  (with-accessors ((next-fact-id get-next-fact-id)) rete
    (prog1
        (values next-fact-id)
      (incf next-fact-id))))

(defun insert-token (rete token)
  (update-time (get-top-fact token) rete)
  (call-node-right (get-root-node (get-compiler rete)) token))

(defun assert-fact (rete fact)
  (set-fact-id fact (next-fact-id rete))
  (increment-time rete)
  (record-fact rete fact)
  (watchpoint 'assert fact)
  (bind-clos-instance rete fact)
  (insert-token rete (make-add-token :initial-fact fact))
  fact)

(defmethod retract-fact ((self rete) (fact fact))
  (remove-fact self fact)
  (increment-time self)
  (watchpoint 'retract fact)
  (unbind-clos-instance self fact)
  (insert-token self (make-remove-token :initial-fact fact))
  fact)
  
(defmethod retract-fact ((self rete) (fact-id integer))
  (let ((fact (lookup-fact self fact-id)))
    (unless (null fact)
      (retract-fact self fact))
    fact))

(defmethod modify-fact ((self rete) fact slot-changes)
  (insert-token self (make-remove-token :initial-fact fact))
  (mapc #'(lambda (slot)
            (set-slot-value fact (first slot) (second slot)))
        slot-changes)
  (insert-token self (make-add-token :initial-fact fact))
  fact)

(defmethod mark-clos-instance-as-changed ((self rete) instance
                                          &optional (slot-id nil))
  (let ((fact (find-fact-using-instance self instance)))
    (cond ((null fact)
           (warn "This instance is not known to LISA: ~S." instance))
          (t
           (insert-token self (make-remove-token :initial-fact fact))
           (synchronize-with-instance fact slot-id)
           (insert-token self (make-add-token :initial-fact fact))))
    instance))

(defun set-initial-state (rete)
  (remove-facts rete)
  (remove-activations (get-strategy rete))
  (setf (get-next-fact-id rete) 0)
  (setf (get-clock rete) 0)
  (setf (slot-value rete 'fired-rule-count) 0)
  t)

(defun assert-autofacts (rete)
  (mapc #'(lambda (deffact)
            (mapc #'(lambda (fact)
                      (assert-fact rete (make-fact-from-template fact)))
                  (get-deffacts deffact)))
        (get-autofacts rete)))

(defun reset-engine (rete)
  (insert-token rete (make-clear-token
                      :initial-fact (get-clear-fact rete)))
  (set-initial-state rete)
  (assert (initial-fact))
  (assert-autofacts rete)
  t)

(defun bind-clos-instance (rete fact)
  (setf (gethash (instance-of-fact fact)
                 (get-instance-table rete)) fact))

(defun unbind-clos-instance (rete fact)
  (remhash (instance-of-fact fact)
           (get-instance-table rete)))

(defun find-fact-using-instance (rete instance)
  (gethash instance (get-instance-table rete)))

(defun forget-clos-instances (rete)
  (clrhash (get-instance-table rete)))
  
(defmethod clear-engine ((self rete))
  (forget-clos-instances self)
  (set-initial-state self)
  (remove-rules self)
  (remove-autofacts self)
  (setf (slot-value self 'compiler) (make-rete-compiler))
  t)

(defun get-rule-list (rete)
  (let ((rules (list)))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (push val rules))
             (get-rules rete))
    rules))

(defun get-activation-list (rete)
  (list-activations (get-strategy rete)))

(defmethod run-engine ((self rete) &optional (step t))
  (flet ((prepare-for-run ()
           (setf (slot-value self 'halted-p) nil)))
    (let ((strategy (get-strategy self)))
      (prepare-for-run)
      (do ((count 0))
          ((or (eql count step) (engine-halted-p self)) count)
        (let ((activation (next-activation strategy)))
          (cond ((null activation)
                 (halt-engine self))
                ((eligible-p activation)
                 (incf (slot-value self 'fired-rule-count))
                 (fire-rule activation)
                 (incf count))))))))

(defun halt-engine (rete)
  (setf (slot-value rete 'halted-p) t))

(defun make-rete (strategy)
  (make-instance 'rete :strategy strategy))
