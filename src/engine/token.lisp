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

;;; $Id: token.lisp,v 1.27 2001/02/22 21:26:12 youngde Exp $

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
              :accessor get-negation-count)
   (clock :initform 0
          :accessor get-clock))
  (:documentation
   "A TOKEN is the fundamental unit of communication in the Rete
   network. TOKENs represent one or more facts, and subclasses of
   TOKEN represent network operations (eg. ADD, REMOVE)."))

(defmethod get-top-fact ((self token))
  (get-fact self))

(defmethod get-all-facts ((self token))
  (labels ((collect-facts (token list)
             (if (null token)
                 (values list)
               (collect-facts (get-parent token)
                              (nconc list `(,(get-top-fact token)))))))
    (delete-if #'(lambda (fact)
                   (= (get-fact-id fact) -1))
               (collect-facts self nil))))

(defun find-fact (token level)
  (declare (type token token) (type integer level))
  (let ((fact-token token))
    (dotimes (i (- (get-depth token) level))
      (setf fact-token (get-parent fact-token)))
    (cl:assert (not (null fact-token)) ())
    (get-top-fact fact-token)))

#+ignore
(defun find-fact (token level)
  (declare (type token token) (type integer level))
  (let ((fact-token token))
    (do ((i (- (get-depth token) level)))
        ((not (plusp (decf i))))
      (setf fact-token (get-parent fact-token)))
    (cl:assert (not (null fact-token)) ())
    (get-top-fact fact-token)))

(defmethod increment-negation-count ((self token))
  (incf (get-negation-count self)))

(defmethod decrement-negation-count ((self token))
  (with-accessors ((count get-negation-count)) self
    (cl:assert (> count 0))
    (decf count)))

(defmethod is-negated-p ((self token))
  (> (get-negation-count self) 0))

(defmethod size ((self token))
  (get-depth self))

(defmethod update-time ((self token) engine)
  (decf (get-clock self)
        (+ (get-time (get-top-fact self))
           (get-engine-time engine))))

(defmethod equals ((self token) (tok token))
  (let ((rval (and (eql (get-fact-id (get-fact self))
                        (get-fact-id (get-fact tok)))
                   (eql (get-sort-code self)
                        (get-sort-code tok))
;                   (equals (get-top-fact self)
;                           (get-top-fact tok))
                   (or (eql (get-parent self)
                            (get-parent tok))
                       (and (not (null (get-parent self)))
                            (not (null (get-parent tok)))
                            (equals (get-parent self)
                                    (get-parent tok)))))))
    #+nil(when rval
      (break "These two tokens are apparently equal..."))
    (values rval)))

(defmethod hash-code ((self token))
  (get-sort-code self))

(defmethod initialize-instance :after ((self token)
                                       &key (initial-fact nil)
                                            (parent nil) (clone nil))
  (flet ((init-new (initial-fact)
           (setf (slot-value self 'fact) initial-fact)
           (setf (slot-value self 'clock) (get-time initial-fact))
           (setf (slot-value self 'sort-code) (get-fact-id initial-fact)))
         (init-derived (fact parent)
           (setf (slot-value self 'fact) fact)
           (setf (slot-value self 'parent) parent)
           (setf (slot-value self 'depth) (1+ (get-depth parent)))
           (setf (slot-value self 'sort-code) 
             (+ (ash (get-sort-code parent) 3)
                (get-fact-id fact)))
           (setf (slot-value self 'clock) (+ (get-time fact)
                                            (get-clock parent))))
         (init-clone (token)
           (setf (slot-value self 'fact) (get-top-fact token))
           (setf (slot-value self 'parent) (get-parent token))
           (setf (slot-value self 'depth) (get-depth token))
           (setf (slot-value self 'sort-code) (get-sort-code token))
           (setf (slot-value self 'clock) (get-clock token))
           (setf (slot-value self 'neg-count) (get-negation-count token))))
    (cond ((and (not (null parent))
                (not (null initial-fact)))
           (init-derived initial-fact parent))
          ((not (null clone))
           (init-clone clone))
          ((not (null initial-fact))
           (init-new initial-fact))
          (t
           (error "In INITIALIZE-INSTANCE for class ~S: inconsistent keyword arguments."
                  self)))
    (when (next-method-p)
      (call-next-method))))

(defmethod print-object ((self token) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(~S)" (get-top-fact self))))

(defun make-token (class &key (initial-fact nil) (parent nil) (clone nil))
  (make-instance class
    :initial-fact initial-fact :parent parent :clone clone))

(defun make-new-token (class initial-fact)
  (make-token class :initial-fact initial-fact))

(defun make-derived-token (class parent initial-fact)
  (make-token class :parent parent :initial-fact initial-fact))

(defun make-clone-token (class token)
  (make-token class :clone token))
