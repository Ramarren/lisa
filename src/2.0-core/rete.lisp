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

;;; $Id: rete.lisp,v 1.8 2002/09/20 18:52:14 youngde Exp $

(in-package "LISA")

(defclass rete ()
  ((meta-data :reader rete-meta-data
              :initform (make-meta-data))
   (rule-table :initform (make-hash-table)
               :reader rete-rule-table)
   (fact-table :initform (make-hash-table)
               :reader rete-fact-table)
   (network :initform (make-rete-network)
            :reader rete-network)
   (next-fact-id :initform -1
                 :accessor rete-next-fact-id)
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
  (meta-data-fact-map (get-meta-data rete)))

(defun meta-class-map (rete)
  (meta-data-class-map (get-meta-data rete)))

(defmethod add-new-rule ((self rete) rule)
  (setf (gethash (rule-name rule) (rete-rule-table rete)) rule)
  rule)

(defun record-fact (rete fact)
  (setf (gethash (fact-id fact) (rete-fact-table rete)) fact))

(defun lookup-fact (rete fact-id)
  (gethash id (rete-fact-table rete)))

(defun remove-fact (rete fact)
  (remhash (fact-id fact) (rete-fact-table rete)))

(defun remove-facts (rete)
  (clrhash (rete-fact-table rete)))

(defun make-fact-list (rete)
  (sort
   (loop for fact being the hash-value of (rete-fact-table rete)
       collect fact)
   #'(lambda (f1 f2)
       (< (fact-id f1) (fact-id f2)))))

(defun next-fact-id (rete)
  (incf (rete-fact-id rete)))

(defmethod assert-fact ((self rete) fact)
  (set-fact-id fact (next-fact-id self))
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

(defun reset-engine ((self rete))
  (reset-network (rete-network self))
  (set-initial-state rete)
  (assert (initial-fact))
  (assert-autofacts rete)
  t)

(defun get-activation-list (rete)
  (list-activations (get-strategy rete)))

(defmethod run-engine ((self rete) &optional (step -1))
  (flet ((prepare-for-run ()
           (setf (slot-value self 'halted-p) nil)))
    (let ((strategy (rete-strategy self)))
      (prepare-for-run)
      (do ((count 0))
          ((or (= count step) (engine-halted-p self)) count)
        (let ((activation (next-activation strategy)))
          (cond ((null activation)
                 (halt-engine self))
                ((eligible-p activation)
                 (increment-fired-rule-count self)
                 (fire-activation activation)
                 (incf count))))))))

(defun halt-engine (rete)
  (setf (slot-value rete 'halted-p) t))

(defun make-rete (strategy)
  (make-instance 'rete :strategy strategy))

(defun make-inference-engine ()
  (make-rete nil))
