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

;;; $Id: mab.lisp,v 1.12 2001/01/23 15:26:16 youngde Exp $

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

;;; Chest-unlocking rules...

(defrule hold-chest-to-put-on-floor
  (?goal (goal-is-to (action unlock) (argument-1 ?chest)))
  (thing (name ?chest) (on-top-of (not floor)) (weight light))
  (monkey (holding (not ?chest)))
  (not (goal-is-to (action hold) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?chest))))

(defrule put-chest-on-floor
  (goal-is-to (action unlock) (argument-1 ?chest))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding ?chest)))
  (?thing (thing (name ?chest)))
  =>
  (format t "Monkey throws the ~A off the ~A onto the floor.~%" ?chest ?on)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor)))

#+ignore
(defrule get-key-to-unlock
  (goal-is-to (action unlock) (argument-1 ?obj))
  (thing (name ?obj) (on-top-of floor))
  (chest (name ?obj) (unlocked-by ?key))
  (monkey (holding (not ?key)))
  (not (goal-is-to (action hold) (argument-1 ?key)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?key))))

#+ignore
(defrule move-to-chest-with-key
  (goal-is-to (action unlock) (argument-1 ?chest))
  (thing (name ?chest) (location ?cplace) (on-top-of floor))
  (monkey (location (not ?cplace)) (holding ?key))
  (chest (name ?chest) (unlocked-by ?key))
  (not (goal-is-to (action walk-to) (argument-1 ?cplace)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?cplace))))

#+ignore
(defrule unlock-chest-with-key
  (?goal (goal-is-to (action unlock) (argument-1 ?name)))
  (?chest (chest (name ?name) (contents ?contents) (unlocked-by ?key)))
  (thing (name ?name) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of ?on) (holding ?key))
  =>
  (format t "Monkey opends the ~A with the ~A revealing the ~A.~%"
          ?name ?key ?contents)
  (modify ?chest (contents nothing))
  (assert (thing (name ?contents) (location ?place) (on-top-of ?name)))
  (retract ?goal))

;;; Hold-object rules...

#+ignore
(defrule unlock-chest-to-hold-object
  (goal-is-to (action hold) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action unlock) (argument-1 ?chest))))

#+ignore
(defrule use-ladder-to-hold
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (not (thing (name ladder) (location ?place)))
  (not (goal-is-to (action move) (argument-1 ladder) (argument-2 ?place)))
  =>
  (assert (goal-is-to (action move) (argument-1 ladder) (argument-2 ?place))))

#+ignore
(defrule climb-ladder-to-hold
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ceiling) (weight light))
  (thing (name ladder) (location ?place) (on-top-of floor))
  (monkey (on-top-of (not ladder)))
  (not (goal-is-to (action on) (argument-1 ladder)))
  =>
  (assert (goal-is-to (action on) (argument-1 ladder))))

#+ignore
(defrule grab-object-from-ladder
  (?goal (goal-is-to (action hold) (argument-1 ?name)))
  (?thing (thing (name ?name) (location ?place) 
                 (on-top-of ceiling) (weight light)))
  (thing (name ladder) (location ?place))
  (?monkey (monkey (location ?place) (on-top-of ladder) (holding blank)))
  =>
  (format t "Monkey grabs the ~A.~%" ?name)
  (modify ?thing (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

#+ignore
(defrule climb-to-hold
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place (not ceiling))
         (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of (not ?on)))
  (not (goal-is-to (action on) (argument-1 ?on)))
  =>
  (assert (goal-is-to (action on) (argument-1 ?on))))

#+ignore
(defrule walk-to-hold
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of (not ceiling))
         (weight light))
  (monkey (location (not ?place)))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place))))

#+ignore
(defrule drop-to-hold
  (goal-is-to (action hold) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on) (weight light))
  (monkey (location ?place) (on-top-of ?on) (holding (not blank)))
  (not (goal-is-to (action hold) (argument-1 blank)))
  =>
  (assert (goal-is-to (action hold) (argument-1 blank))))

#+ignore
(defrule grab-object
  (?goal (goal-is-to (action hold) (argument-1 ?name)))
  (?thing (thing (name ?name) (location ?place) 
                 (on-top-of ?on) (weight light)))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding blank)))
  =>
  (format t "Monkey grabs the ~A.~%" ?name)
  (modify ?thing (location held) (on-top-of held))
  (modify ?monkey (holding ?name))
  (retract ?goal))

#+ignore
(defrule drop-object
  (?goal (goal-is-to (action hold) (argument-1 blank)))
  (?monkey (monkey (location ?place) (on-top-of ?on) 
                   (holding ?name (not blank))))
  (?thing (thing (name ?name)))
  =>
  (format t "Monkey drops the ~A.~%" ?name)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of ?on))
  (retract ?goal))

;;; Move-object rules...

#+ignore
(defrule unlock-chest-to-move-object
  (goal-is-to (action move) (argument-1 ?obj))
  (chest (name ?chest) (contents ?obj))
  (not (goal-is-to (action unlock) (argument-1 ?chest)))
  =>
  (assert (goal-is-to (action unlock) (argument-1 ?chest))))

#+ignore
(defrule hold-object-to-move
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (thing (name ?obj) (location (not ?place)) (weight light))
  (monkey (holding (not ?obj)))
  (not (goal-is-to (action hold) (argument-1  ?obj)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?obj))))

#+ignore
(defrule move-object-to-place
  (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place))
  (monkey (location (not ?place)) (holding ?obj))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place))))

#+ignore
(defrule drop-object-once-moved
  (?goal (goal-is-to (action move) (argument-1 ?name) (argument-2 ?place)))
  (?monkey (monkey (location ?place) (holding ?obj)))
  (?thing (thing (name ?name) (weight light)))
  =>
  (format t "Monkey drops the ~A.~%" ?name)
  (modify ?monkey (holding blank))
  (modify ?thing (location ?place) (on-top-of floor))
  (retract ?goal))

#+ignore
(defrule already-moved-object
  (?goal (goal-is-to (action move) (argument-1 ?obj) (argument-2 ?place)))
  (thing (name ?obj) (location ?place))
  =>
  (retract ?goal))

;;; Walk-to-place rules...

#+ignore
(defrule already-at-place
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (monkey (location ?place))
  =>
  (retract ?goal))

#+ignore
(defrule get-on-floor-to-walk
  (goal-is-to (action walk-to) (argument-1 ?place))
  (monkey (location (not ?place)) (on-top-of (not floor)))
  (not (goal-is-to (action on) (argument-1 floor)))
  =>
  (assert (goal-is-to (action on) (argument-1 floor))))

#+ignore
(defrule walk-holding-nothing
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (?monkey (monkey (location (not ?place)) (on-top-of floor) (holding blank)))
  =>
  (format t "Monkey walks to ~A.~%" ?place)
  (modify ?monkey (location ?place))
  (retract ?goal))

#+ignore
(defrule walk-holding-object
  (?goal (goal-is-to (action walk-to) (argument-1 ?place)))
  (?monkey (monkey (location (not ?place)) (on-top-of floor) (holding ?obj)))
  (thing (name ?obj))
  =>
  (format t "Monkey walks to ~A holding the ~A.~%" ?place ?obj)
  (modify ?monkey (location ?place))
  (retract ?goal))

;;; Get-on-object rules...

#+ignore
(defrule jump-onto-floor
  (?goal (goal-is-to (action on) (argument-1 floor)))
  (?monkey (monkey (on-top-of ?on (not floor))))
  =>
  (format t "Monkey jumps off the ~A onto the floor.~%" ?on)
  (modify ?monkey (on-top-of floor))
  (retract ?goal))

#+ignore
(defrule walk-to-place-to-climb
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location (not ?place)))
  (not (goal-is-to (action walk-to) (argument-1 ?place)))
  =>
  (assert (goal-is-to (action walk-to) (argument-1 ?place))))

#+ignore
(defrule drop-to-climb
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place))
  (monkey (location ?place) (holding (not blank)))
  (not (goal-is-to (action hold) (argument-1 blank)))
  =>
  (assert (goal-is-to (action hold) (argument-1 blank))))

#+ignore
(defrule climb-indirectly
  (goal-is-to (action on) (argument-1 ?obj))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  (monkey (location ?place) (on-top-of (not (or* ?on ?obj)))
          (holding blank))
  (not (goal-is-to (action on) (argument-1 ?on)))
  =>
  (assert (goal-is-to (action on) (argument-1 ?on))))

#+ignore
(defrule climb-directly
  (?goal (goal-is-to (action on) (argument-1 ?obj)))
  (thing (name ?obj) (location ?place) (on-top-of ?on))
  (?monkey (monkey (location ?place) (on-top-of ?on) (holding blank)))
  =>
  (format t "Monkey climbs onto the ~A.~%" ?obj)
  (modify ?monkey (on-top-of ?obj))
  (retract ?goal))

#+ignore
(defrule already-on-object
  (?goal (goal-is-to (action on) (argument-1 ?obj)))
  (monkey (on-top-of ?obj))
  =>
  (retract ?goal))

;;; Eat-object rules...

#+ignore
(defrule hold-to-eat
  (goal-is-to (action eat) (argument-1 ?obj))
  (monkey (holding (not ?obj)))
  (not (goal-is-to (action hold) (argument-1 ?obj)))
  =>
  (assert (goal-is-to (action hold) (argument-1 ?obj))))

#+ignore
(defrule satisfy-hunger
  (?goal (goal-is-to (action eat) (argument-1 ?name)))
  (?monkey (monkey (holding ?name)))
  (?thing (thing (name ?name)))
  =>
  (format t "Monkey eats the ~A.~%" ?name)
  (modify ?monkey (holding blank))
  (retract ?goal)
  (retract ?thing))

;;; startup rule...

#+ignore
(defrule startup
  =>
  (assert (monkey (location t5-7) (on-top-of green-couch) (holding blank)))
  (assert (thing (name green-couch) (location t5-7) (weight heavy)))
  (assert (thing (name red-couch) (location t2-2) (weight heavy)))
  (assert (thing (name big-pillow) (location t2-2) (on-top-of red-couch)))
  (assert (thing (name red-chest) (location t2-2) (on-top-of big-pillow)))
  (assert (chest (name red-chest) (contents ladder) (unlocked-by red-key)))
  (assert (thing (name blue-chest) (location t7-7) (on-top-of ceiling)))
  (assert (thing (name grapes) (location t7-8) (on-top-of ceiling)))
  (assert (chest (name blue-chest) (contents bananas) (unlocked-by blue-key)))
  (assert (thing (name blue-couch) (location t8-8) (weight heavy)))
  (assert (thing (name green-chest) (location t8-8) (on-top-of ceiling)))
  (assert (chest (name green-chest) (contents blue-key) (unlocked-by red-key)))
  (assert (thing (name red-key) (location t1-3)))
  (assert (goal-is-to (action eat) (argument-1 bananas))))

