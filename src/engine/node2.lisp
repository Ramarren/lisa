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

;;; File: node2.lisp
;;; Description: A non-negated, two-input node of the Rete network. Tests in
;;; this node compare slot values and types in facts from the left and right
;;; inputs.

;;; $Id: node2.lisp,v 1.2 2000/11/07 01:57:50 youngde Exp $

(in-package :lisa)

(defclass node2 (node-test)
  ((left-tree :initform (make-token-tree)
              :accessor get-left-tree)
   (right-tree :initform (make-token-tree)
               :accessor get-right-tree)
   (hashkey :initform nil
            :accessor get-hashkey))
  (:documentation
   "Description: A non-negated, two-input node of the Rete network. Tests in
   this node compare slot values and types in facts from the left and right
   inputs."))

(defmethod add-to-left-tree ((self node2) token)
  (add-token (get-left-tree self) token))

(defmethod add-to-right-tree ((self node2) token)
  (add-token (get-right-tree self) token))

(defmethod call-node-left ((self node2) (token add-token))
  (add-to-left-tree self token)
  (run-tests-vary-right self token (get-right-tree self)))

(defmethod call-node-right ((self node2) (token add-token))
  (add-to-right-tree self token)
  (run-tests-vary-left self token (get-left-tree self)))

(defmethod run-tests-vary-left ((self node2) right-token tree)
  (labels ((eval-tests (left-token)
             (when (map-until #'do-tests (get-tests self))
               (pass-along self (make-token left-token right-token))))
           (eval-tokens (tokens)
             (mapcar #'eval-tests tokens)))
    (maphash #'eval-tokens tree)))

(defmethod run-tests-vary-right ((self node2) token tree)
  (labels ((eval-right-tests (token tests)))
    (maphash #'eval-right-tests tree)))

(defun make-node2 (engine hash)
  (make-instance 'node2 :engine engine :hashkey hash))

