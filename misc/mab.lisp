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

;;; $Id: mab.lisp,v 1.5 2001/01/15 22:06:42 youngde Exp $

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

(defrule put-chest-on-floor
  (goal-is-to (action unlock) (argument-1 ?chest))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding ?chest)))
  (?thing (thing (name ?chest)))
  =>
  (format t "Monkey throws the ~A off the ~A onto the floor.~%" ?chest ?on)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor)))

(defrule get-key-to-unlock
  (goal-is-to (action unlock) (argument-1 ?obj))
  (thing (name ?obj) (on-top-of floor))
  (chest (name ?obj) (unlocked-by ?key))
  (monkey (holding (not ?key)))
  (not (goal-is-to (action hold) (argument-1 ?key)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?key))))

(defrule move-to-chest-with-key
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (location ?cplace) (on-top-of floor))
  (monkey (location (not ?cplace)) (holding ?key))
  (chest (name ?chest) (unlocked-by ?key))
  (not (goal-is-to (action walk-to) (argument-1 ?cplace)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?cplace))))

(defrule unlock-chest-to-hold-object
  (goal-is-to (action hold) (argument-1 ?obj))
  (not (goal-is-to (action unlock)))
  =>
  (format t "unlock-chest-to-hold-object fired: ?obj = ~S~%" ?obj))
