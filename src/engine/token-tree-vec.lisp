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

;;; File: token-tree.lisp
;;; Description: Maintains a hashed collection of tokens.

;;; $Id: token-tree-vec.lisp,v 1.1 2001/03/27 18:34:00 youngde Exp $

(in-package "LISA")

(defclass token-tree ()
  ((table :initform (make-array 101
                                :initial-element nil
                                :element-type 'list)
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
  (push token (aref (get-table self)
                    (create-hash-code self token)))
  (incf (get-size self))
  (values token))

(defun remove-token (self token)
  (declare (type token-tree self) (type token token))
  (let* ((table (get-table self))
         (hash (create-hash-code self token))
         (tokens (aref table hash))
         (foundp nil))
    (unless (null tokens)
      (setf (aref table hash)
        (delete-if #'(lambda (tok)
                       (setf foundp (equals tok token)))
                   tokens :count 1)))
    (when foundp
      (decf (get-size self)))
    (values foundp)))

(defun use-sortcode-p (self)
  (declare (type token-tree self))
  (get-use-sortcode self))

(defun create-hash-code (tree token)
  (declare (type token-tree tree) (type token token))
  (mod (abs (if (use-sortcode-p tree)
                (hash-code token)
              (get-fact-id (find-fact token 0))))
       101))

(defun token-tree-count (self)
  (declare (type token-tree self))
  (get-size self))

(defun clear-tree (self)
  (declare (type token-tree self))
  (fill (get-table self) nil))

(defmacro with-tree-iterator ((key token token-tree) &body body)
  (let ((table (gensym)))
    `(let ((,table (get-table ,token-tree)))
      (dotimes (i 101)
        (unless (null (aref ,table i))
          (mapc #'(lambda (,token)
                    ,@body)
                (aref ,table i)))))))

(defun make-token-tree (&key (use-sortcode-p nil))
  (make-instance 'token-tree :use-sortcode use-sortcode-p))
