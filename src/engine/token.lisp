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

;;; File: token.lisp
;;; Description: A TOKEN is the fundamental unit of communication in
;;; the Rete network. TOKENs represent one or more facts, and
;;; subclasses of TOKEN represent network operations (eg. ADD,
;;; REMOVE).

;;; $Id: token.lisp,v 1.4 2000/11/08 20:49:00 youngde Exp $

(in-package :lisa)

(defclass token ()
  ((sort-code :initform 0
              :reader get-sort-code)
   (fact :initform nil
         :initarg :fact
         :reader get-fact)
   (parent :initform nil
           :accessor get-parent)
   (depth :initform 0
          :reader get-depth)
   (neg-count :initform 0
              :accessor get-neg-count)
   (clock :initform 0
          :accessor get-clock))
  (:documentation
   "A TOKEN is the fundamental unit of communication in the Rete
   network. TOKENs represent one or more facts, and subclasses of
   TOKEN represent network operations (eg. ADD, REMOVE)."))

(defmethod get-top-fact ((self token))
  (get-fact self))

(defmethod find-fact ((self token) level)
  (labels ((traverse (token level)
             (cond ((<= level 0)
                    (get-parent token))
                   (t
                    (traverse (get-parent token) (1- level))))))
    (traverse self (- (get-depth self) level))))

(defmethod size ((self token))
  (get-depth self))

(defmethod update-time ((self token) (engine rete))
  (decf (get-clock self)
        (+ (get-time (get-top-fact self))
           (get-time engine))))

(defmethod equals ((self token) (tok token))
  (eql (get-fact-id (get-fact self))
       (get-fact-id (get-fact tok))))

(defmethod initialize-instance :after ((self token)
                                       &key fact (parent nil) (clone nil))
  (flet ((init-derived (self fact parent)
           (setf (slot-value self 'depth) (1+ (get-depth parent)))
           (setf (slot-value self 'sort-code) 
             (+ (ash (get-sort-code parent) 3)
                (get-fact-id fact)))
           (setf (slot-value self 'clock) (+ (get-time fact)
                                            (get-clock parent))))
         (init-new (self fact)
           (setf (slot-value self 'clock) (get-time fact))
           (setf (slot-value self 'sort-code) (get-fact-id fact)))
         (init-clone (self token)
           (setf (slot-value self 'fact) (get-top-fact token))
           (setf (slot-value self 'parent) (get-parent token))
           (setf (slot-value self 'depth) (get-depth token))
           (setf (slot-value self 'sort-code) (get-sort-code token))
           (setf (slot-value self 'clock) (get-clock token))
           (setf (slot-value self 'neg-count) (get-neg-count token))))
    (cond ((not (null clone))
           (init-clone self clone))
          ((not (null parent))
           (init-derived self parent))
          (t (init-new self)))
    (when (next-method-p)
      (call-next-method))))

(defun make-token (initial-fact &key (parent nil) (clone nil))
  (make-instance 'token :fact initial-fact :parent parent :clone clone))
