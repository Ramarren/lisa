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

;;; $Id: factories.lisp,v 1.13 2001/01/10 21:37:42 youngde Exp $

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

(defun make-node1 (slot pattern rule)
  (declare (type (slot slot) (pattern pattern) (rule rule)))
  (flet ((reject-binding (binding)
           (or (not (typep binding 'slot-binding))
               (> (get-location binding)
                  (get-location pattern)))))
    (make-node1-tfn (get-name slot)
                    (make-function-call 
                     `(,(get-constraint slot))
                     (remove-if #'reject-binding (get-binding-list rule))))))
