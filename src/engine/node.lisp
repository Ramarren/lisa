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

;;; File: node.lisp
;;; Description: Parent class of all nodes in the Rete pattern network. This
;;; class is intended to be abstract.

;;; $Id: node.lisp,v 1.11 2000/12/14 18:18:12 youngde Exp $

(in-package :lisa)

(defclass node ()
  ((use-count :initform 0
              :accessor get-use-count)
   (successors :initform nil
               :accessor get-successors))
  (:documentation
   "Parent class of all nodes in the Rete pattern network. This class is
   intended to be abstract."))

(defgeneric call-node-left (node token))
(defgeneric call-node-right (node token))

(defgeneric equals (node-1 node-2)
  (:method ((n1 node) n2)
           (values nil)))

(defmethod increase-use-count ((self node))
  (incf (get-use-count self)))

(defmethod decrease-use-count ((self node))
  (decf (get-use-count self)))

(defmethod add-successor ((self node) node rule)
  "Adds a node to the list of successors."
  (with-accessors ((successors get-successors)) self
    (setf successors (nconc successors `(,node)))
    (add-node rule node))
  (values))

(defmethod remove-successor ((self node) node)
  (with-accessors ((successors get-successors)) self
    (setf successors
      (delete node successors :test #'equals))))

(defmethod merge-successor ((self node) node rule)
  "Merges a node into the list of successors. Returns either NODE or the
  existing successor."
  (labels ((merge-maybe (successors)
             (let ((test (first successors)))
               (cond ((null test)
                      (add-successor self node rule)
                      (values node))
                     ((equals node test)
                      (add-node rule test)
                      (values test))
                     (t
                      (merge-maybe (rest successors)))))))
    (merge-maybe (get-successors self))))

(defmethod resolve ((self node) node)
  (or (find-if #'(lambda (n)
                   (equals n node))
               (get-successors self))
      node))
