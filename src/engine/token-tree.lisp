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

;;; File: token-tree.lisp
;;; Description: Maintains a collection of tokens. Each hash location
;;; yields a LIST of tokens.

;;; $Id: token-tree.lisp,v 1.5 2000/11/18 02:42:11 youngde Exp $

(in-package :lisa)

(defclass token-tree ()
  ((table :initform (make-hash-table)
          :accessor get-table))
  (:documentation
   "Maintains a hashed collection of tokens. Each hash location yields
   a LIST of tokens."))

(defmethod add-token ((self token-tree) (tok token))
  (with-accessors ((table get-table)) self
    (let* ((key (make-hash-code self tok))
           (slot (gethash key table)))
      (setf (gethash key table)
        (nconc slot `(,tok))))))

(defmethod remove-token ((self token-tree) (tok token))
  (with-accessors ((table get-table)) self
    (let* ((key (make-hash-code self tok))
           (slot (gethash key table)))
      (setf (gethash key table)
        (delete tok slot :test #'equals)))))

(defmethod clear-tree ((self token-tree))
  (clrhash (get-table self)))

(defmethod make-hash-code ((self token-tree) token)
  (sxhash (get-sort-code token)))

(defmethod token-tree-count ((self token-tree))
  (hash-table-count (get-table self)))

(defmacro with-token-tree-iterator ((mname tree) &body body)
  `(with-hash-table-iterator (,mname (get-table tree))
     ,@body))

(defun maptree (function tree)
  (maphash function (get-table tree)))

(defun make-token-tree ()
  (make-instance 'token-tree))
