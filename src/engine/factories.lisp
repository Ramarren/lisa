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

;;; File: factories.lisp
;;; Description: Factory code responsible for creating various types
;;; of LISA entities.

;;; $Id: factories.lisp,v 1.22 2001/02/02 18:20:59 youngde Exp $

(in-package :lisa)

(defun make-pattern (pp location)
  (let ((head (first (parsed-pattern-pattern pp)))
        (body (second (parsed-pattern-pattern pp))))
    (case (parsed-pattern-type pp)
      (:generic
       (make-generic-pattern head body location
                             (parsed-pattern-binding pp)))
      (:negated
       (make-not-pattern head body location))
      (otherwise
       (error "The Pattern factory doesn't this raw pattern type ~S."
              (parsed-pattern-type pp))))))

(defmethod make-join-node ((pattern generic-pattern) engine)
  (make-node2 engine))

(defmethod make-join-node ((pattern not-pattern) engine)
  (make-node2-not engine))

(defmethod make-join-node (pattern engine)
  (declare (ignore engine))
  (error "The Join Node factory doesn't understand this pattern type: ~S~%"
         (class-of pattern)))

(defun make-node-function-call (slot pattern)
  (make-function-call `(,(get-constraint slot)) (get-bindings pattern)))

(defmethod make-node1 ((slot optimisable-variable-slot) pattern)
  (declare (ignore pattern))
  (if (is-negatedp slot)
      (make-node1-neq (get-name slot) (get-constraint slot))
    (make-node1-teq (get-name slot) (get-constraint slot))))

(defmethod make-node1 ((slot optimisable-literal-slot) pattern)
  (declare (ignore pattern))
  (if (is-negatedp slot)
      (make-node1-neq (get-name slot) (get-value slot))
    (make-node1-teq (get-name slot) (get-value slot))))

(defmethod make-node1 ((slot slot) pattern)
  (make-node1-tfn (get-name slot)
                  (make-node-function-call slot pattern)))

(defmethod make-node2-test ((slot optimisable-variable-slot) pattern)
  (let ((binding (lookup-binding pattern (get-value slot))))
    (if (is-negatedp slot)
        (make-test2-neq (get-location binding) (get-slot-name binding)
                        (get-name slot))
      (make-test2-eq (get-location binding) (get-slot-name binding)
                     (get-name slot)))))

(defmethod make-node2-test ((slot slot) pattern)
  (make-test2-eval (make-node-function-call slot pattern)))
