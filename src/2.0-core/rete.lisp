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

;;; $Id: rete.lisp,v 1.9 2002/09/20 21:28:45 youngde Exp $

(in-package "LISA")

(defclass rete ()
  ((meta-data :reader rete-meta-data
              :initform (make-meta-data))
   (rule-table :initform (make-hash-table)
               :reader rete-rule-table)
   (fact-table :initform (make-hash-table)
               :reader rete-fact-table)
   (instance-table :initform (make-hash-table)
                   :reader rete-instance-table)
   (network :initform (make-rete-network)
            :reader rete-network)
   (next-fact-id :initform -1
                 :accessor rete-next-fact-id)
   (autofacts :initform (list)
              :accessor rete-autofacts)
   (halted :initform nil
           :accessor rete-halted)
   (firing-count :initform 0
                 :accessor rete-firing-count)
   (strategy :initarg :strategy
             :initform nil
             :reader rete-strategy)))

(defmethod initialize-instance :after ((self rete) &rest args)
  (initialize-internal-concepts self))

(defun initialize-internal-concepts (rete)
  (with-inference-engine (rete)
    (deftemplate initial-fact ())
    rete))

(defun meta-fact-map (rete)
  (meta-data-fact-map (rete-meta-data rete)))

(defun meta-class-map (rete)
  (meta-data-class-map (rete-meta-data rete)))

(defmethod add-new-rule ((self rete) rule)
  (setf (gethash (rule-name rule) (rete-rule-table self)) rule)
  rule)

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
   #'(lambda (f1 f2)
       (< (fact-id f1) (fact-id f2)))))

(defun next-fact-id (rete)
  (incf (rete-next-fact-id rete)))

(defun add-autofact (rete deffact)
  (push deffact (rete-autofacts rete)))

(defun remove-autofacts (rete)
  (setf (rete-autofacts rete) nil))

(defun assert-autofacts (rete)
  (mapc #'(lambda (deffact)
            (mapc #'(lambda (fact)
                      (assert-fact rete (make-fact-from-template fact)))
                  (deffacts-fact-list deffact)))
        (rete-autofacts rete)))

(defmethod assert-fact ((self rete) fact)
  (setf (fact-id fact) (next-fact-id self))
  (remember-fact self fact)
  (add-fact-to-network (rete-network self) fact)
  fact)

(defmethod retract-fact ((self rete) (fact fact))
  (forget-fact self fact)
  (remove-fact-from-network (rete-network self) fact)
  fact)

(defmethod retract-fact ((self rete) (fact-id integer))
  (let ((fact (find-fact-by-id self fact-id)))
    (and (not (null fact))
         (retract-fact self fact))))

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
  (let ((fact (find-fact-using-instance self instance)))
    (cond ((null fact)
           (warn "This instance is not known to LISA: ~S." instance))
          (t
           ;;(insert-token self (make-remove-token :initial-fact fact))
           (synchronize-with-instance fact slot-id)))
           ;;(insert-token self (make-add-token :initial-fact fact))))
    instance))

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

(defun make-rete (strategy)
  (make-instance 'rete :strategy strategy))

(defun make-inference-engine ()
  (make-rete (make-breadth-first-strategy)))
