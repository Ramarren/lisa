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

;;; $Id: token-tree.lisp,v 1.14 2001/02/09 15:19:56 youngde Exp $

(in-package :lisa)

(defclass token-tree ()
  ((table :initform (make-hash-table)
          :accessor get-table)
   (use-sortcode-p :initarg :use-sortcode
                   :initform nil
                   :accessor get-use-sortcode)
   (size :initform 0
         :accessor get-size))
  (:documentation
   "Maintains a hashed collection of tokens."))

(defun add-token (self token)
  (declare (type token-tree self) (type token token))
  (let* ((hash (create-hash-code self token))
         (table (get-table self))
         (tokens (gethash hash table)))
    (setf (gethash hash table)
          (push token tokens))
    (incf (get-size self))
    (values)))

(defun remove-token (self token)
  (declare (type token-tree self) (type token token))
  (with-accessors ((table get-table)) self
    (let* ((key (create-hash-code self token))
           (token-list (gethash key table))
           (foundp nil))
      (setf token-list
            (delete-if #'(lambda (obj)
                           (setf foundp (equals obj token)))
                       token-list :count 1))
      (when foundp
        (if (null token-list)
            (remhash key table)
          (setf (gethash key table) token-list))
        (decf (get-size self)))
      (values foundp))))

(defun use-sortcode-p (self)
  (declare (type token-tree self))
  (get-use-sortcode self))

(defun create-hash-code (tree token)
  (declare (type token-tree tree) (type token token))
  (if (use-sortcode-p tree)
      (hash-code token)
    (get-fact-id (get-top-fact token))))

(defun clear-tree (self)
  (declare (type token-tree self))
  (clrhash (get-table self))
  (setf (get-size self) 0))

(defun token-tree-count (self)
  (declare (type token-tree self))
  (get-size self))

#+ignore
(defmacro with-tree-iterator ((key value tree) &body body)
  "Iterates over each element in TREE (an instance of TOKEN-TREE) and
  evaluates BODY. RETURN may be used to exit from the iterator with
  specified results."
  (let ((generator (gensym))
        (foundp (gensym)))
    `(with-hash-table-iterator (,generator (get-table ,tree))
       (loop
         (multiple-value-bind (,foundp ,key ,value)
             (,generator)
           (if ,foundp
               (progn ,@body)
             (return nil)))))))

(defmacro with-tree-iterator ((key value tree) &body body)
  "Iterates over each element in TREE (an instance of TOKEN-TREE) and
  evaluates BODY. RETURN may be used to exit from the iterator with
  specified results."
  (let ((generator (gensym))
        (foundp (gensym))
        (token-list (gensym))
        (token (gensym))
        (rval (gensym)))
    `(with-hash-table-iterator (,generator (get-table ,tree))
       (loop
         (multiple-value-bind (,foundp ,key ,token-list)
             (,generator)
           (if ,foundp
               (let ((,rval
                      (dolist (,token ,token-list)
                        (setf ,value ,token)
                        ,@body)))
                 (when ,rval
                   (return ,rval)))
             (return nil)))))))

(defun make-token-tree (&key (use-sortcode-p nil))
  (make-instance 'token-tree :use-sortcode use-sortcode-p))
