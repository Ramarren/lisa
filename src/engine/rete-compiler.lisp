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

;;; $Id: rete-compiler.lisp,v 1.32 2001/01/10 21:37:42 youngde Exp $

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
  ((root-node :initform (make-root-node)
              :reader get-root-node)
   (terminals :initform nil
              :accessor get-terminals)
   (roots :initform nil
          :accessor get-roots))
  (:documentation
   "Generates the Rete pattern network."))

(defun add-simple-tests (pattern rule parent-node)
  (let ((last-node parent-node))
    (mapc #'(lambda (slot)
              (when (or (is-localized-patternp pattern)
                        (= (get-pattern-count rule) 1))
                (setf last-node
                  (merge-successor last-node
                                   (make-node1 slot pattern rule)
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

(defun find-multiple-references (compiler pattern rule)
  (let ((occurrences (make-hash-table)))
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
                            (and (typep test 'test1-var)
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
  (with-accessors ((terminals get-terminals)) compiler
    (mapc #'(lambda (pattern)
              (find-multiple-references compiler pattern rule))
          (get-patterns rule))
    (setf (aref terminals 0)
      (merge-successor (aref terminals 0)
                       (make-node1-rtl) rule))))

(defmethod add-node2-test ((test test1-var) rule pattern slot node2)
  (let ((binding (find-binding rule (get-value test))))
    (cl:assert (typep binding 'slot-binding))
    (unless (= (get-location binding)
               (get-location pattern))
      (add-binding-test node2 binding 
                        (get-name slot)))))

(defmethod add-node2-test ((test test1-eval) rule pattern slot node2)
  (flet ((make-bindings ()
           (let ((bindings (list)))
             (maphash #'(lambda (key binding)
                          (declare (ignore key))
                          (when (and (typep binding 'slot-binding)
                                     (<= (get-location binding)
                                         (get-location pattern)))
                            (push binding bindings)))
                      (get-bindings rule))
             (values bindings))))
    (make-test2-eval
     (make-function-call
      `(,(get-value test)) (make-bindings)))))

(defun add-node2-tests (rule node2 pattern)
  (mapc #'(lambda (slot)
            (mapc #'(lambda (test)
                      (add-node2-test test rule pattern slot node2))
                  (get-tests slot)))
        (get-slots pattern))
  (values node2))

;;; the "third pass"...

(defun create-join-nodes (compiler rule)
  (flet ((add-join-node (node location)
           (with-accessors ((terminals get-terminals)) compiler
             (add-successor (aref terminals (1- location)) node rule)
             (add-successor (aref terminals location) node rule)
             (add-node rule node)
             (setf (aref terminals (1- location)) node)
             (setf (aref terminals location) node)
             (values node))))
    (mapc #'(lambda (pattern)
              (let ((node2 (make-join-node pattern (get-engine rule))))
                (add-node2-tests rule node2 pattern)
                (add-join-node node2 (get-location pattern))))
          (rest (get-patterns rule)))))

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
  (search-for-multiple-variables self rule)
  (create-join-nodes self rule)
  (create-terminal-node self rule)
  (values rule))
                                  
(defun make-rete-compiler ()
  (make-instance 'rete-compiler))
