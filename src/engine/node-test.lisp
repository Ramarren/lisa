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

;;; File: node-test.lisp
;;; Description: Node containing an arbitrary list of tests. Used for TEST
;;; conditional elements and as the base class for JOIN nodes.

;;; $Id: node-test.lisp,v 1.20 2001/03/26 16:27:23 youngde Exp $

(in-package "LISA")

(defclass node-test (node)
  ((tests :initform nil
          :accessor get-tests)
   (engine :initarg :engine
           :reader get-engine))
  (:documentation
   "Node containing an arbitrary list of tests. Used for TEST conditional
   elements and as the base class for JOIN nodes."))

(defmethod add-test ((self node-test) test)
  (with-accessors ((tests get-tests)) self
    (setf tests (nconc tests `(,test)))))

(defmethod get-test-count ((self node-test))
  (length (get-tests self)))

(defmethod has-tests-p ((self node-test))
  (plusp (get-test-count self)))

(defmethod call-node-right ((self node-test) (token clear-token))
  (values nil))

(defmethod call-node-right ((self node-test) token)
  (declare (ignore token))
  (call-next-method))

(defun pass-the-token (self token)
  (declare (type node-test self) (type token token))
  (with-accessors ((engine get-engine)) self
    (let ((descendant (make-token (get-null-fact engine) :parent token)))
      (update-time descendant engine)
      (pass-along self descendant)
      (values t))))

#+ignore
(defmethod call-node-left ((self node-test) (token add-token))
  (if (run-tests self token)
      (pass-the-token self token)
    (values nil)))

(defmethod call-node-left ((self node-test) (token add-token))
  (break))

(defmethod call-node-left ((self node-test) (token clear-token))
  (pass-along self token)
  (values t))

(defmethod call-node-left ((self node-test) (token remove-token))
  (pass-the-token self token))

(defmethod run-tests ((self node-test) token fact)
  (every #'(lambda (test) (do-test test token fact))
         (get-tests self)))

(defmethod pass-along ((self node-test) token)
  (mapcar #'(lambda (node)
              (call-node-left node token))
          (get-successors self)))

(defmethod equals ((self node-test) (obj node-test))
  (eq self obj))
