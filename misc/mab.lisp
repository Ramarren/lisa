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

;;; File: mab.lisp
;;; Description: The "Monkey And Bananas" sample implementation, a common AI
;;; planning problem. The monkey's objective is to find and eat some bananas.

;;; $Id: mab.lisp,v 1.4 2001/01/13 21:00:28 youngde Exp $

(in-package :lisa)

(defclass monkey ()())
(defclass thing ()())
(defclass chest ()())
(defclass goal-is-to ()())

(defimport monkey lisa::monkey)
(defimport thing lisa::thing)
(defimport chest lisa::chest)
(defimport goal-is-to lisa::goal-is-to)

(watch :activations)

(defrule hold-chest-to-put-on-floor
  (?goal (goal-is-to (action unlock) (argument-1 ?chest)))
  (thing (name ?chest) (on-top-of (not floor)) (weight light))
  (monkey (holding (not ?chest)))
  (not (goal-is-to (action hold) (argument-1 ?chest)))
  =>
  (retract ?goal)
  (assert (goal-is-to (action hold) (argument-1 ?chest))))

(defrule unlock-chest-to-hold-object
  (goal-is-to (action hold) (argument-1 ?obj))
  (not (goal-is-to (action unlock)))
  =>
  (format t "unlock-chest-to-hold-object fired: ?obj = ~S~%" ?obj))
