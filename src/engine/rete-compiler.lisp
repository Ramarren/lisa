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
;;; Description: This class generates a pattern network.

;;; See the paper "Rete: A Fast Algorithm for the Many Pattern/Many
;;; Object Pattern Match Problem", Charles L. Forgy, Artificial
;;; Intelligence 19 (1982), 17-37.

;;; The implementation used here is an adaptation of that used in the
;;; Java Expert System Shell (JESS), Ernest Friedman-Hill, Sandia
;;; National Labs. As such, it does not follow Forgy's paper. Rather,
;;; LISA "models the Rete net more literally as a set of networked
;;; Node objects with interconnections."

;;; $Id: rete-compiler.lisp,v 1.5 2000/11/14 20:45:58 youngde Exp $

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
              :reader get-root-node))
  (:documentation
   "Generates a pattern network."))

(defmethod add-rule ((self rete-compiler) rule)
  "Adds a rule to the pattern network."
  (let ((terminals 
         (make-array (get-pattern-count rule))
        (rule-roots
         (make-array (get-pattern-count rule)))))
    (labels ((first-pass (patterns &optional (i 0))
               (let ((pattern (first patterns)))
                 (cond ((null pattern)
                        (values t))
                       (t
                        (let ((last (merge-successor
                                     (get-root-node self)
                                     (make-node1-tect
                                      (get-name pattern)) rule)))
                          (setf (aref rule-roots i) last)
                          (find-multifields)
                          (setf (aref terminals i)
                            (add-tests last)))))))))))
               
               
               
             
