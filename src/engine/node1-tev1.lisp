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

;;; File: node1-tev1.lisp
;;; Description: This class represents network nodes that test whether
;;; two slots in the same fact have the same type and value.

;;; $Id: node1-tev1.lisp,v 1.3 2000/12/07 02:20:22 youngde Exp $

(in-package :lisa)

(defclass node1-tev1 (node1)
  ((initial-slot :initarg :initial-slot
                 :reader get-initial-slot)
   (reference-slot :initarg :reference-slot
                   :reader get-reference-slot))
  (:documentation
   "This class represents network nodes that test whether two slots in
   the same fact have the same type and value."))

(defmethod call-node-right ((self node1-tev1) token)
  (cond ((call-next-method self token)
         (values nil))
        (t
         (let ((fact (get-top-fact token)))
           (cond ((equal (get-slot-value fact (get-initial-slot self))
                         (get-slot-value fact (get-reference-slot self)))
                  (pass-along self token)
                  (values t))
                 (t (values nil)))))))

(defmethod equals ((self node1-tev1) (node node1-tev1))
  (and (eq (get-initial-slot self)
           (get-initial-slot node))
       (eq (get-reference-slot self)
           (get-reference-slot node))))
  
(defmethod print-object ((self node1-tev1) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(initial-slot = ~S ; reference-slot = ~S)"
            (get-initial-slot self) (get-reference-slot self))))

(defun make-node1-tev1 (init-slot ref-slot)
  (make-instance 'node1-tev1 :initial-slot init-slot
                 :reference-slot ref-slot))

