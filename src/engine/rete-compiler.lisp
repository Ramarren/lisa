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

;;; File: rete-compiler.lisp
;;; Description: This class generates the Rete pattern network.

;;; See the paper "Rete: A Fast Algorithm for the Many Pattern/Many
;;; Object Pattern Match Problem", Charles L. Forgy, Artificial
;;; Intelligence 19 (1982), 17-37.

;;; The implementation used here is an adaptation of that used in the
;;; Java Expert System Shell (JESS), Ernest Friedman-Hill, Sandia
;;; National Labs. As such, it does not follow Forgy's paper. Rather,
;;; LISA "models the Rete net more literally as a set of networked
;;; Node objects with interconnections."

;;; $Id: rete-compiler.lisp,v 1.53 2001/03/02 21:50:50 youngde Exp $

(in-package :lisa)

(defclass root-node (node1)
  ()
  (:documentation
   "Private class used by RETE-COMPILER to represent root nodes in the
   network."))

(defmethod call-node-right ((self root-node) token)
  (pass-along self token))

(defun make-root-node ()
  (make-instance 'root-node))

(defclass rete-compiler ()
  ((root-node :reader get-root-node)
   (terminals :initform nil
              :accessor get-terminals)
   (roots :initform nil
          :accessor get-roots))
  (:documentation
   "Generates the Rete pattern network."))

(defmethod initialize-instance :after ((self rete-compiler) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'root-node) (make-root-node)))

(defmacro simple-slotp (slot)
  `(or (typep ,slot 'optimisable-literal-slot)
       (typep ,slot 'optimisable-simple-constraint-slot)
       (and (has-complex-constraintp ,slot)
            (localized-slotp ,slot))))

(defun add-simple-tests (pattern rule parent-node)
  (let ((last-node parent-node))
    (mapc #'(lambda (slot)
              (when (or (simple-slotp slot)
                        (= (get-pattern-count rule) 1))
                (setf last-node
                  (merge-successor last-node
                                   (make-node1 slot pattern)
                                   rule))))
          (get-slots pattern))
    (values last-node)))

(defun create-single-nodes (compiler rule patterns)
  (with-accessors ((terminals get-terminals)
                   (roots get-roots)) compiler
    (labels ((first-pass (patterns i)
               (let ((pattern (first patterns)))
                 (cond ((null pattern)
                        (values t))
                       (t
                        (let ((last (merge-successor
                                     (get-root-node compiler)
                                     (make-node1-tect
                                      (get-name pattern)) rule)))
                          (setf (aref roots i) last)
                          (setf (aref terminals i)
                            (add-simple-tests pattern rule last))
                          (first-pass (rest patterns) (1+ i))))))))
      (first-pass patterns 0))))

(defun add-right-to-left-node (compiler rule)
  (with-accessors ((terminals get-terminals)) compiler
    (setf (aref terminals 0)
      (merge-successor (aref terminals 0) (make-node1-rtl) rule))))
    
#+ignore
(defun add-node2-tests (node2 pattern)
  (flet ((add-constraint-test (slot)
           (add-test node2
                     (make-test2-eval
                      (make-node-function-call slot pattern)))))
    (mapc #'(lambda (slot)
              (unless (or (localized-slotp slot)
                          (not (has-constraintp slot)))
                (add-constraint-test slot)))
          (get-slots pattern))
    (values node2)))

(defun add-node2-tests (node2 pattern)
  (labels ((first-variable-occurrence (var)
             (let ((binding (lookup-binding pattern var)))
               (cl:assert (not (null binding)) ()
                          "Missing binding for ~S" var)
               (= (get-location pattern) (get-location binding))))
           (add-constraint-test (slot)
             (unless (and (first-variable-occurrence (get-value slot))
                          (not (has-constraintp slot)))
               (let ((node (make-node2-test slot pattern)))
                 (unless (null node)
                   (add-test node2 node))))))
    (mapc #'(lambda (slot)
              (unless (simple-slotp slot)
                (add-constraint-test slot)))
          (get-slots pattern))
    (values node2)))

;;; the "third pass"...

(defun create-join-nodes (compiler rule)
  (with-accessors ((terminals get-terminals)) compiler
    (labels ((add-join-node (node2 location)
               (add-successor (aref terminals (1- location)) node2 rule)
               (add-successor (aref terminals location) node2 rule)
               (add-node rule node2)
               (setf (aref terminals (1- location)) node2)
               (setf (aref terminals location) node2)))
      (mapc #'(lambda (pattern)
                (let ((node2 (make-join-node pattern (get-engine rule))))
                  (add-node2-tests node2 pattern)
                  (add-join-node node2 (get-location pattern))))
            (rest (get-patterns rule))))))

(defun create-terminal-node (compiler rule)
  (merge-successor
   (aref (get-terminals compiler) (1- (get-pattern-count rule)))
   (make-terminal-node rule) rule))

(defmethod add-rule-to-network ((self rete-compiler) rule)
  "Adds a rule to the pattern network."
  (freeze-rule rule)
  (setf (get-terminals self) (make-array (get-pattern-count rule)))
  (setf (get-roots self) (make-array (get-pattern-count rule)))
  (create-single-nodes self rule (get-patterns rule))
  (add-right-to-left-node self rule)
  (create-join-nodes self rule)
  (create-terminal-node self rule)
  (values rule))
                                  
(defun make-rete-compiler ()
  (make-instance 'rete-compiler))

