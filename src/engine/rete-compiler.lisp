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

;;; $Id: rete-compiler.lisp,v 1.63 2001/04/21 18:18:52 youngde Exp $

(in-package "LISA")

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
  `(or (typep ,slot 'optimisable)
       (and (typep ,slot 'complex-slot)
            (localized-slotp ,slot))))

(defgeneric add-simple-tests (pattern rule parent-node)
  (:method (pattern rule parent-node)
           (declare (ignore rule parent-node))
           (error
            "The Rete compiler doesn't understand this pattern type: ~S."
            pattern)))

(defmethod add-simple-tests ((self generic-pattern) rule parent-node)
  (let ((last-node parent-node))
    (mapc #'(lambda (slot)
              (when (or (simple-slotp slot)
                        (and (= (get-pattern-count rule) 1)
                             (not (typep slot 'variable-slot))))
                (setf last-node
                  (merge-successor last-node
                                   (make-node1 slot self)
                                   rule))))
          (get-slots self))
    (values last-node)))

(defmethod add-simple-tests ((self test-pattern) rule parent-node)
  (declare (ignore rule))
  (values parent-node))

#+ignore
(defun create-single-nodes (compiler rule patterns)
  (declare (optimize (speed 3) (debug 1) (safety 1)))
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

(defgeneric generate-single-node (pattern)
  (:method (pattern)
           (error
            "The Rete compiler doesn't understand this pattern type: ~S."
            pattern)))

(defmethod generate-single-node ((self generic-pattern))
  (make-node1-tect (get-name self)))

(defmethod generate-single-node ((self test-pattern))
  (make-node1-nop))
  
(defun create-single-nodes (compiler rule patterns)
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (with-accessors ((terminals get-terminals)
                   (roots get-roots)) compiler
    (labels ((first-pass (patterns i)
               (let ((pattern (first patterns)))
                 (cond ((null pattern)
                        (values t))
                       (t
                        (let ((last
                               (merge-successor (get-root-node compiler)
                                                (generate-single-node pattern)
                                                rule)))
                          (setf (aref roots i) last)
                          (setf (aref terminals i)
                            (add-simple-tests pattern rule last))
                          (first-pass (rest patterns) (1+ i))))))))
      (first-pass patterns 0))))

(defun add-right-to-left-node (compiler rule)
  (with-accessors ((terminals get-terminals)) compiler
    (setf (aref terminals 0)
      (merge-successor (aref terminals 0) (make-node1-rtl) rule))))
    
(defun add-node2-tests (node2 pattern)
  (labels ((first-variable-occurrence (var)
             (let ((binding (lookup-binding pattern var)))
               (cl:assert (not (null binding)) ()
                          "Missing binding for ~S" var)
               (= (get-location pattern) (get-location binding))))
           (add-constraint-test (slot)
             (unless (and (first-variable-occurrence (get-value slot))
                          (not (typep slot 'constraint-slot)))
               (let ((node (make-node2-test slot pattern)))
                 (add-test node2 node)))))
    (mapc #'(lambda (slot)
              (unless (simple-slotp slot)
                (add-constraint-test slot)))
          (get-slots pattern))
    (values node2)))

;;; the "third pass"...

(defgeneric generate-join-node (pattern rule)
  (:method (pattern rule)
           (declare (ignore rule))
           (error "The Rete compiler doesn't understand this pattern type: ~S"
                  pattern)))

(defmethod generate-join-node ((pattern generic-pattern) rule)
  (let ((node2 (make-join-node pattern (get-engine rule))))
    (add-node2-tests node2 pattern)
    (values node2)))

(defmethod generate-join-node ((pattern test-pattern) rule)
  (let ((node2 (make-node-test (get-engine rule))))
    (add-test node2 
              (make-test2-eval
               (make-function-call `(,(get-form pattern) )
                                   (get-bindings pattern))))
    (values node2)))

(defun create-join-nodes (compiler rule)
  (with-accessors ((terminals get-terminals)) compiler
    (labels ((add-join-node (node2 location)
               (add-successor (aref terminals (1- location)) node2 rule)
               (add-successor (aref terminals location) node2 rule)
               (add-node rule node2)
               (setf (aref terminals (1- location)) node2)
               (setf (aref terminals location) node2)))
      (mapc #'(lambda (pattern)
                (add-join-node
                 (generate-join-node pattern rule) (get-location pattern)))
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

