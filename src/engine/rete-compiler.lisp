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

;;; $Id: rete-compiler.lisp,v 1.13 2000/12/05 01:03:17 youngde Exp $

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
  ((hash-key :initform 101
             :accessor get-hash-key)
   (root-node :initform (make-root-node)
              :reader get-root-node)
   (terminals :initform nil
              :accessor get-terminals)
   (roots :initform nil
          :accessor get-roots))
  (:documentation
   "Generates the Rete pattern network."))

(defun add-tests (slots rule node)
  (flet ((add-simple-test (node slot)
           (merge-successor 
            node (make-node1-teq (get-name slot)
                                 (get-value (first (get-tests slot)))) rule)))
    (if (null slots)
        (values node)
      (add-tests (rest slots) rule
                 (add-simple-test node (first slots))))))
           
(defun create-single-nodes (compiler rule patterns)
  (with-accessors ((terminals get-terminals)
                   (roots get-roots)) compiler
    (labels ((find-multifields ()
               (values nil))
             (first-pass (patterns i)
               (let ((pattern (first patterns)))
                 (cond ((null pattern)
                        (values t))
                       (t
                        (let ((last (merge-successor
                                     (get-root-node compiler)
                                     (make-node1-tect
                                      (get-name pattern)) rule)))
                          (setf (aref roots i) last)
                          (find-multifields)
                          (setf (aref terminals i)
                            (add-tests (get-slots pattern) rule last))
                          (first-pass (rest patterns) (1+ i))))))))
      (first-pass patterns 0))))

(defun search-for-variables (compiler rule)
  (with-accessors ((terminals get-terminals)) compiler
    (setf (aref terminals 0)
      (merge-successor (aref terminals 0)
                       (make-node1-rtl) rule))))

(defun create-join-nodes (compiler rule)
  (labels ((add-join-node (node i)
             (with-accessors ((terminals get-terminals)) compiler
               (add-successor (aref terminals (1- i)) node rule)
               (add-successor (aref terminals i) node rule)
               (add-node rule node)
               (setf (aref terminals (1- i)) node)
               (setf (aref terminals i) node)
               (values node)))
           (third-pass (patterns i)
             (cond ((null patterns)
                    (values t))
                   (t
                    (add-join-node (make-node2 (get-engine rule)) i)
                    (third-pass (rest patterns) (1+ i))))))
    (third-pass (rest (get-patterns rule)) 1)))

(defun create-terminal-node (compiler rule)
  (with-accessors ((terminals get-terminals)) compiler
    (setf (aref terminals 0)
      (merge-successor (aref terminals 0)
                       (make-terminal-node rule) rule))))

(defmethod add-rule-to-network ((self rete-compiler) rule)
  "Adds a rule to the pattern network."
  (freeze-rule rule)
  (format t "pattern count: ~D~%" (get-pattern-count rule))
  (setf (get-terminals self) (make-array (get-pattern-count rule)))
  (setf (get-roots self) (make-array (get-pattern-count rule)))
  (create-single-nodes self rule (get-patterns rule))
  (search-for-variables self rule)
  (create-join-nodes self rule)
  (create-terminal-node self rule)
  (values rule))
                                  
(defun make-rete-compiler ()
  (make-instance 'rete-compiler))
