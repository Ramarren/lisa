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

;;; File: sample.lisp
;;; Description: Some simple classes useful in early Lisa testing.

;;; $Id: sample.lisp,v 1.58 2001/02/09 01:49:59 youngde Exp $

(in-package :lisa)

(defclass rocky ()
  ((name :initarg :name
         :initform nil
         :reader get-name)))

(defclass boris ()
  ((name :initarg :name
         :initform nil
         :reader get-name)))

(defclass natasha ()
  ((name :initarg :name
         :initform nil
         :reader get-name)
   (nemesis :initarg :nemesis
            :initform nil
            :reader get-nemesis)))

(defclass fearless-leader () ())
  
(defun make-rocky (name)
  (make-instance 'rocky :name name))

(defun make-boris (name)
  (make-instance 'boris :name name))

(defun make-natasha (name nemesis)
  (make-instance 'natasha :name name :nemesis nemesis))

(defimport rocky lisa::rocky)
(defimport boris lisa::boris)
(defimport natasha lisa::natasha)
(defimport fearless-leader lisa::fearless-leader)

(watch :activations)

#+ignore
(defrule all-slot-types
  (rocky (name rocky))
  (rocky (name (not boris)))
  (boris (name ?name))
  (boris (name (not ?name)))
  (boris (name ?name boris))
  (boris (name ?name (not rocky)))
  (natasha (nemesis ?nemesis (not ?name)))
  (natasha (nemesis ?nemesis (eql ?nemesis rocky)))
  =>
  (break))

#+ignore
(defrule fearless-leader
  (fearless-leader (type fearless))
  =>
  (format t "fearless-leader fired.~%"))

#+ignore
(defrule rocky
  (rocky (name "rocky"))
  =>
  (format t "rocky-1 fired.~%"))

#+ignore
(defrule rocky-not
  (rocky (name (not "rocky")))
  (boris (name "boris"))
  =>
  (format t "rocky-not fired.~%"))

(defrule nemesis
  (natasha (name "natasha") (nemesis ?nemesis "rocky"))
  (rocky (name (not ?nemesis)))
  =>
  (format t "nemesis fired! Value of ?nemesis is ~S~%" ?nemesis))

#+ignore
(defrule boris
  (boris (name ?name "boris"))
  =>
  (format t "boris fired (name = ~S).~%" ?name))

#+ignore
(defrule multiple-references
  (natasha (name ?name "natasha") (nemesis ?name))
  =>
  (format t "multiple-references fired! Value of ?name is ~S~%" ?name))

#+ignore
(defrule schtum
  (?f-1 (rocky (name "rocky")))
  =>
  (format t "schtum fired (?f-1 = ~S).~%" ?f-1)
  (modify ?f-1 (name "bullwinkle")))

#+ignore
(defrule rocky-boris-natasha
  (?f-1 (rocky (name "bullwinkle")))
  (boris (name ?boris-name "boris"))
  (natasha (name "natasha") (nemesis ?boris-name))
  =>
  (format t "rocky-boris-natasha fired (?boris-name = ~S).~%" ?boris-name)
  (retract ?f-1))

#+ignore
(defrule not-pattern
  (boris (name ?name "boris"))
  (natasha (name ?name))
  (not (rocky (name "bullwinkle")))
  =>
  (format t "not-pattern fired.~%"))

#+ignore
(defrule no-patterns
    =>
  (format t "no-patterns fired!~%"))

(defrule simple-predicate
  (?fact (boris (name ?name (string= ?name "boris"))))
  =>
  (format t "simple-predicate fired: ?name = ~S~%" ?name)
  (retract ?fact))

#+ignore
(defrule any-rocky
  (?fact (rocky (name ?name)))
  =>
  (format t "any-rocky fired: ?name = ~S~%" ?name)
  (retract ?fact))

#+ignore
(defrule any-rocky-but-boris
  (?fact (rocky (name ?name (not "boris"))))
  =>
  (format t "any-rocky-but-boris fired: ?name = ~S~%" ?name)
  (retract ?fact))
