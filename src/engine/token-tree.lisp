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
;;; Description: Maintains a hashed collection of tokens.

;;; $Id: token-tree.lisp,v 1.10 2000/12/06 00:36:20 youngde Exp $

(in-package :lisa)

(defclass token-tree ()
  ((table :initform (make-hash-table)
          :accessor get-table)
   (use-sortcode-p :initarg :use-sortcode
                   :initform nil
                   :accessor get-use-sortcode))
  (:documentation
   "Maintains a hashed collection of tokens."))

(defmethod add-token ((self token-tree) (tok token))
  (setf (gethash (create-hash-code self tok)
                 (get-table self)) tok))

(defmethod remove-token ((self token-tree) (tok token))
  (with-accessors ((table get-table)) self
    (let* ((key (create-hash-code self tok))
           (obj (gethash key table)))
      (unless (null obj)
        (remhash key table))
      (values obj))))

(defmethod use-sortcode-p ((self token-tree))
  (get-use-sortcode self))

(defmethod create-hash-code ((self token-tree) token)
  (cond ((use-sortcode-p self)
         (hash-code token))
        (t
         (get-fact-id (get-top-fact token)))))

(defmethod clear-tree ((self token-tree))
  (clrhash (get-table self)))

(defmethod token-tree-count ((self token-tree))
  (hash-table-count (get-table self)))

(defmacro with-tree-iterator ((key value tree) &body body)
  "Iterates over each element in TREE (an instance of TOKEN-TREE) and
  evaluates BODY. RETURN may be used to exit from the iterator with
  specified results."
  (let ((generator (gensym))
        (foundp (gensym)))
    `(with-hash-table-iterator (,generator ,tree)
       (loop
         (multiple-value-bind (,foundp ,key ,val)
             (,generator)
           (if ,foundp
               (progn ,@body)
             (return nil)))))))

(defun make-token-tree (&key (use-sortcode-p nil))
  (make-instance 'token-tree :use-sortcode use-sortcode-p))
