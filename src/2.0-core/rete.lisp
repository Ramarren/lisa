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

;;; $Id: rete.lisp,v 1.4 2002/08/28 20:18:34 youngde Exp $

(in-package "LISA")

(defclass rete ()
  ((meta-data :reader get-meta-data
              :initform (make-meta-data))
   (rule-table :initform (make-hash-table)
               :reader get-rule-table)
   (strategy :initarg :strategy
             :initform nil)))

(defun meta-fact-map (rete)
  (meta-data-fact-map (get-meta-data rete)))

(defun meta-class-map (rete)
  (meta-data-class-map (get-meta-data rete)))

(defun add-rule (rete rule)
  (setf (gethash (get-name rule) (get-rule-table rete)) rule)
  rule)

(defun assert-fact (rete fact)
  fact)

(defun make-rete (strategy)
  (make-instance 'rete :strategy strategy))

(defun make-inference-engine ()
  (make-rete nil))
