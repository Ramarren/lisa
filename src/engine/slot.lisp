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

;;; File: slot.lisp
;;; Description: Represents a single slot within a pattern.

;;; $Id: slot.lisp,v 1.24 2001/04/16 18:31:54 youngde Exp $

(in-package "LISA")

(defclass slot ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (value :initarg :value
          :accessor get-value)
   (negated :initarg :negated
            :initform nil
            :reader get-negated)
   (locality :initform t
             :reader get-locality))
  (:documentation
   "Represents a single slot within a pattern."))

(defun is-negatedp (self)
  (declare (type slot self))
  (get-negated self))

(defun localized-slotp (slot)
  (get-locality slot))

(defun slot-has-global-binding (slot)
  (setf (slot-value slot 'locality) nil))

(defmethod print-object ((self slot) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; value = ~S ; negated = ~S)"
            (get-name self) (get-value self)
            (get-negated self))))

(defclass optimisable () ())

(defclass literal-slot (slot optimisable)
  ()
  (:documentation
   "A subclass of SLOT describing instances that contain no variables."))

(defclass variable-slot (slot)
  ()
  (:documentation
   "A subclass of SLOT describing instances that contain variables."))

(defclass constraint-slot (slot)
  ((constraint :initarg :constraint
               :reader get-constraint))
  (:documentation
   "A subclass of SLOT describing instances that have constraints. Possession
   of a constraint implies a variable value."))

(defmethod print-object ((self constraint-slot) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(name = ~S ; value = ~S ; constraint = ~S ; negated = ~S)"
            (get-name self) (get-value self)
            (get-constraint self) (get-negated self))))

(defclass simple-variable-slot (variable-slot) ())
(defclass simple-constraint-slot (constraint-slot optimisable) ())
(defclass complex-slot (constraint-slot) ())

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

(defun make-slot (name value constraint)
  "Factory function that constructs instances of various slot types."
  (macrolet ((generate-test (var value negated)
               `(if ,negated
                    `(not (equal ,,var ,,value))
                  `(equal ,,var ,,value))))
    (cond ((literalp value)
           (make-instance 'literal-slot :name name :value value))
          ((negated-literalp value)
           (make-instance 'literal-slot
             :name name :value (second value) :negated t))
          ((negated-variablep value)
           (make-instance 'simple-variable-slot
             :name name :value (second value) :negated t))
          ;; Then the slot value must be a variable...
          ((null constraint)
           (make-instance 'simple-variable-slot :name name :value value))
          ;; The value is a variable and there is a constraint...
          ((literalp constraint)
           (make-instance 'simple-constraint-slot 
             :name name :value value :constraint constraint))
          ((negated-literalp constraint)
           (make-instance 'simple-constraint-slot
             :name name :value value 
             :constraint (second constraint) :negated t))
          ((variablep constraint)
           (make-instance 'complex-slot :name name :value value
                          :constraint (generate-test value constraint nil)))
          ((negated-variablep constraint)
           (make-instance 'complex-slot :name name :value value
                          :constraint (generate-test value (second constraint) t)
                          :negated t))
          ;; This must be a complex slot with user-written Lisp code as the
          ;; constraint...
          (t
           (make-instance 'complex-slot :name name :value value
                          :constraint constraint)))))
