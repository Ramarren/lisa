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

;;; $Id: rete-compiler.lisp,v 1.18 2000/12/06 22:52:37 youngde Exp $

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
   (varname-workspace :initform (make-hash-table)
                      :accessor get-varname-workspace)
   (terminals :initform nil
              :accessor get-terminals)
   (roots :initform nil
          :accessor get-roots))
  (:documentation
   "Generates the Rete pattern network."))

(defun add-simple-tests (slots rule node)
  (labels ((add-test-maybe (node slot-name tests)
             (let ((test (first tests)))
               (cond ((null test)
                      (values node))
                     (t
                      (if (value-is-variable-p test)
                          (add-test-maybe node slot-name (rest tests))
                        (add-test-maybe (merge-successor
                                         node (make-node1-teq slot-name
                                                              (get-value test))
                                         rule)
                         slot-name (rest tests)))))))
           (add-tests (node slot)
             (add-test-maybe node (get-name slot) (get-tests slot))))
    (if (null slots)
        (values node)
      (add-simple-tests (rest slots) rule
                  (add-tests node (first slots))))))
           
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
                            (add-simple-tests (get-slots pattern) rule last))
                          (first-pass (rest patterns) (1+ i))))))))
      (first-pass patterns 0))))

(defun find-multiple-references (compiler pattern rule)
  (with-accessors ((occurrences get-varname-workspace)) compiler
    (labels ((first-occurrence (varname)
               (not (gethash varname occurrences)))
             (add-occurrence (varname)
               (setf (gethash varname occurrences) varname))
             (add-multiple-reference-test (slot test reference)
               (with-accessors ((terminals get-terminals)) compiler
                 (let* ((location (get-location pattern))
                        (node (aref terminals location))
                        (ref-slot (car reference))
                        (ref-test (cdr reference)))
                   (setf (aref terminals location)
                     (merge-successor node (make-node1-tev1 (get-name slot)
                                                            (get-name ref-slot))
                                      rule)))))
             (add-multiple-reference-tests (slot test references)
               (mapc #'(lambda (ref)
                         (add-multiple-reference-test slot test ref))
                     references))
             (find-initial-reference (slot)
               (find-if #'(lambda (test)
                            (and (value-is-variable-p test)
                                 (first-occurrence (get-value test))))
                        (get-tests slot)))
             (find-remaining-references (slots varname references)
               (let ((slot (first slots)))
                 (cond ((null slot)
                        (values references))
                       (t
                        (let ((test (find-if #'(lambda (test)
                                                 (eq (get-value test) varname))
                                             (get-tests slot))))
                          (find-remaining-references
                           (rest slots) varname
                           (if (null test)
                               references
                             (nconc references
                                    `(,(cons slot test))))))))))
             (find-multiple-references-aux (slots)
               (let ((slot (first slots)))
                 (cond ((null slot)
                        (values))
                       (t
                        (let ((test (find-initial-reference slot)))
                          (unless (null test)
                            (add-occurrence (get-value test))
                            (add-multiple-reference-tests 
                             slot test (find-remaining-references
                                   (rest slots) (get-value test) nil)))
                          (find-multiple-references-aux (rest slots))))))))
      (find-multiple-references-aux (get-slots pattern)))))
  
(defun search-for-multiple-variables (compiler rule)
  (with-accessors ((terminals get-terminals)
                   (occurrences get-varname-workspace)) compiler
    (mapc #'(lambda (pattern)
              (clrhash occurrences)
              (find-multiple-references compiler pattern rule))
          (get-patterns rule))
    (setf (aref terminals 0)
      (merge-successor (aref terminals 0)
                       (make-node1-rtl) rule))))

(defun add-node2-tests (rule node2 slots)
  (labels ((add-tests (slot tests)
             (let ((test (first tests)))
               (cond ((null test)
                      (values))
                     ((value-is-variable-p test)
                      (let ((binding (find-binding rule (get-value test))))
                        (cl:assert (typep binding 'slot-binding))
                        (add-binding-test node2 binding (get-name slot))
                        (add-tests slot (rest tests))))
                     (t
                      (add-tests slot (rest tests)))))))
    (unless (null slots)
      (let ((slot (first slots)))
        (add-tests slot (get-tests slot))
        (add-node2-tests rule node2 (rest slots))))
    (values node2)))
                      
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
                    (let ((node2 (make-node2 (get-engine rule))))
                      (add-node2-tests rule node2 (get-slots (first patterns)))
                      (add-join-node node2 i)
                      (third-pass (rest patterns) (1+ i)))))))
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
  (search-for-multiple-variables self rule)
  (create-join-nodes self rule)
  (create-terminal-node self rule)
  (values rule))
                                  
(defun make-rete-compiler ()
  (make-instance 'rete-compiler))
