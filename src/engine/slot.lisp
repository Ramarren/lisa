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

;;; File: slot.lisp
;;; Description: Represents a single slot within a pattern.

;;; $Id: slot.lisp,v 1.18 2001/03/13 21:56:13 youngde Exp $

(in-package :lisa)

(defclass slot ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (value :initarg :value
          :accessor get-value)
   (constraint :initarg :constraint
               :initform nil
               :accessor get-constraint)
   (negated :initarg :negated
            :initform nil
            :reader get-negated)
   (locality :initform t
             :reader get-locality))
  (:documentation
   "Represents a single slot within a pattern."))

(defun is-literal-slotp (self)
  (declare (type slot self))
  (literalp (get-value self)))

(defun is-variable-slotp (self)
  (declare (type slot self))
  (variablep (get-value self)))

(defun is-negatedp (self)
  (declare (type slot self))
  (get-negated self))

(defun has-constraintp (slot)
  (declare (type slot slot))
  (not (null (get-constraint slot))))

(defun has-complex-constraintp (slot)
  (declare (type slot slot))
  (consp (get-constraint slot)))

(defun localized-slotp (slot)
  (get-locality slot))

(defun slot-has-global-binding (slot)
  (setf (slot-value slot 'locality) nil))

(defmethod print-object ((self slot) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; value = ~S ; constraint = ~S ; negated = ~S)"
            (get-name self) (get-value self)
            (get-constraint self)
            (get-negated self))))

(defclass optimisable-slot (slot)
  ()
  (:documentation
   "A subclass of SLOT describing a slot instance that's eligible for
   certain optimisations."))

(defclass simple-slot () ())
(defclass optimisable-literal-slot (optimisable-slot simple-slot) ())
(defclass optimisable-variable-slot (optimisable-slot) ())
(defclass optimisable-simple-constraint-slot (optimisable-slot simple-slot) ())

(defmacro negated-constraintp (constraint)
  `(and (consp ,constraint)
        (eq (first ,constraint) 'not)
        (not (consp (second ,constraint)))))

(defmacro negated-literalp (constraint)
  `(and (consp ,constraint)
        (eq (first ,constraint) 'not)
        (literalp (second ,constraint))))

(defmacro negated-variablep (constraint)
  `(and (consp ,constraint)
        (eq (first ,constraint) 'not)
        (variablep (second ,constraint))))

(defun make-slot (name value constraint global-bindings)
  "Factory function that constructs instances of various slot types."
  (macrolet ((generate-test (var value negated)
               `(if ,negated
                    `(not (equal ,,var ,,value))
                  `(equal ,,var ,,value))))
    (cond ((literalp value)
           (make-instance 'optimisable-literal-slot :name name :value value))
          ((negated-literalp value)
           (make-instance 'optimisable-literal-slot
             :name name :value (second value) :negated t))
          ((negated-variablep value)
           (make-instance 'optimisable-variable-slot
             :name name :value (second value) :negated t))
          ;; Then the slot value must be a variable...
          ((null constraint)
           (if (lookup-binding global-bindings value)
               (make-instance 'optimisable-variable-slot 
                 :name name :value value)
             (make-instance 'slot :name name :value value)))
          ;; The value is a variable and there is a constraint...
          ((literalp constraint)
           (make-instance 'optimisable-simple-constraint-slot 
             :name name :value value :constraint constraint))
          ((negated-literalp constraint)
           (make-instance 'optimisable-simple-constraint-slot
             :name name :value value 
             :constraint (second constraint) :negated t))
          ((variablep constraint)
           (make-instance 'slot :name name :value value
                          :constraint
                          (generate-test value constraint nil)))
          ((negated-variablep constraint)
           (make-instance 'slot :name name :value value
                          :constraint 
                          (generate-test value (second constraint) t)
                          :negated t))
          ;; This must be a complex slot with user-written Lisp code as the
          ;; constraint...
          (t
           (make-instance 'slot :name name :value value
                          :constraint constraint)))))
