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

;;; File: mab.lisp
;;; Description: The "Monkey And Bananas" sample implementation, a common AI
;;; planning problem. The monkey's objective is to find and eat some bananas.

;;; $Id: mab-kw.lisp,v 1.2 2001/03/23 00:06:05 youngde Exp $

(require "kw")

(in-package "KW-USER")

(def-kb-struct monkey
    location on-top-of holding)

(def-kb-struct thing
    name location on-top-of weight)

(def-kb-struct chest
    name contents unlocked-by)

(def-kb-struct goal-is-to
    action argument-1 argument-2)

(defcontext mab :strategy (priority recency order) :auto-return t)

;;; Chest-unlocking rules...

(defrule hold-chest-to-put-on-floor :forward :context mab
  (goal-is-to ?goal action unlock argument-1 ?chest)
  (thing ?thing name ?chest on-top-of ?top weight light)
  (test (not (eql ?top 'floor)))
  (not (monkey ?monkey holding ?chest))
  (not (goal-is-to ? action hold argument-1 ?chest))
  -->
  ((format t "hold-chest-to-put-on-floor fired~%"))
  (assert (goal-is-to ? action hold argument-1 ?chest)))

(defrule put-chest-on-floor :forward :context mab
  (goal-is-to ? action unlock argument-1 ?chest)
  (monkey ?monkey location ?place on-top-of ?on holding ?chest)
  (thing ?thing name ?chest)
  -->
  ((format t "Monkey throws the ~A off the ~A onto the floor.~%" ?chest ?on))
  (assert ?monkey holding blank)
  (assert ?thing location ?place on-top-of floor))

(defrule get-key-to-unlock :forward :context mab
  (goal-is-to ? action unlock argument-1 ?obj)
  (thing ? name ?obj on-top-of floor)
  (chest ? name ?obj unlocked-by ?key)
  (not (monkey ? holding ?key))
  (not (goal-is-to ? action hold argument-1 ?key))
  -->
  (assert (goal-is-to ? action hold argument-1 ?key)))

(defrule move-to-chest-with-key :forward :context mab
  (goal-is-to ? action unlock argument-1 ?chest)
  (thing ? name ?chest location ?cplace on-top-of floor)
  (monkey ?monkey location ?loc holding ?key)
  (test (not (eql ?loc ?cplace)))
  (chest ? name ?chest unlocked-by ?key)
  (not (goal-is-to ? action walk-to argument-1 ?cplace))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?cplace)))

(defrule unlock-chest-with-key :forward :context mab
  (goal-is-to ?goal action unlock argument-1 ?name)
  (chest ?chest name ?name contents ?contents unlocked-by ?key)
  (thing ? name ?name location ?place on-top-of ?on)
  (monkey ? location ?place on-top-of ?on holding ?key)
  -->
  ((format t "Monkey opens the ~A with the ~A revealing the ~A.~%"
           ?name ?key ?contents))
  (assert (chest ?chest contents nothing))
  (assert (thing ? name ?contents location ?place weight light
                 on-top-of ?name))
  (erase ?goal))

;;; Hold-object rules...

(defrule unlock-chest-to-hold-object :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (chest ? name ?chest contents ?obj)
  (not (goal-is-to ? action unlock argument-1 ?chest))
  -->
  (assert (goal-is-to ? action unlock argument-1 ?chest)))

(defrule use-ladder-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ceiling weight light)
  (not (thing ? name ladder location ?place))
  (not (goal-is-to ? action move argument-1 ladder argument-2 ?place))
  -->
  (assert (goal-is-to ? action move argument-1 ladder argument-2 ?place)))

(defrule climb-ladder-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ceiling weight light)
  (thing ? name ladder location ?place on-top-of floor)
  (not (monkey ? on-top-of ladder))
  (not (goal-is-to ? action on argument-1 ladder))
  -->
  (assert (goal-is-to ? action on argument-1 ladder)))

(defrule grab-object-from-ladder :forward :context mab
  (goal-is-to ?goal action hold argument-1 ?name)
  (thing ?thing name ?name location ?place 
         on-top-of ceiling weight light)
  (thing ? name ladder location ?place)
  (monkey ?monkey location ?place on-top-of ladder holding blank)
  -->
  ((format t "Monkey grabs the ~A.~%" ?name))
  (assert (thing ?thing location held on-top-of held))
  (assert (monkey ?monkey holding ?name))
  (erase ?goal))

(defrule climb-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on
         weight light)
  (test (not (eql ?place 'ceiling)))
  (monkey ? location ?place on-top-of ?top)
  (test (not (eql ?top ?on)))
  (not (goal-is-to ? action on argument-1 ?on))
  -->
  (assert (goal-is-to ? action on argument-1 ?on)))

(defrule walk-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?top weight light)
  (test (not (eql (?top ceiling))))
  (monkey ? location ?loc)
  (test (not (eql ?loc ?place)))
  (not (goal-is-to ? action walk-to argument-1 ?place))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?place)))

(defrule drop-to-hold :forward :context mab
  (goal-is-to ? action hold argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on weight light)
  (monkey ? location ?place on-top-of ?on holding ?hold)
  (test (not (eql ?hold 'blank)))
  (not (goal-is-to ? action hold argument-1 blank))
  -->
  (assert (goal-is-to ? action hold argument-1 blank)))

(defrule grab-object :forward :context mab
  (goal-is-to ?goal action hold argument-1 ?name)
  (thing ?thing name ?name location ?place 
                 on-top-of ?on weight light)
  (monkey ?monkey location ?place on-top-of ?on holding blank)
  -->
  ((format t "Monkey grabs the ~A.~%" ?name))
  (assert (thing ?thing location held on-top-of held))
  (assert (monkey ?monkey holding ?name))
  (erase ?goal))

(defrule drop-object :forward :context mab
  (goal ?goal action hold argument-1 blank)
  (monkey ?monkey location ?place on-top-of ?on
          holding ?name)
  (test (not (eql ?name blank)))
  (thing ?thing name ?name)
  -->
  ((format t "Monkey drops the ~A.~%" ?name))
  (assert (monkey ?monkey holding blank))
  (assert (thing ?thing location ?place on-top-of ?on))
  (erase ?goal))

;;; Move-object rules...

(defrule unlock-chest-to-move-object :forward :context mab
  (goal-is-to ? action move argument-1 ?obj)
  (chest ? name ?chest contents ?obj)
  (not (goal-is-to ? action unlock argument-1 ?chest))
  -->
  (assert (goal-is-to ? action unlock argument-1 ?chest)))

(defrule hold-object-to-move :forward :context mab
  (goal-is-to ? action move argument-1 ?obj argument-2 ?place)
  (thing ? name ?obj location ?loc weight light)
  (test (not (eql ?loc ?place)))
  (monkey ? holding ?hold)
  (test (not (eql ?hold ?obj)))
  (not (goal-is-to ? action hold argument-1  ?obj))
  -->
  (assert (goal-is-to ? action hold argument-1 ?obj)))

(defrule move-object-to-place :forward :context mab
  (goal-is-to ? action move argument-1 ?obj argument-2 ?place)
  (monkey ? location ?loc holding ?obj)
  (test (not (eql ?loc ?place)))
  (not (goal-is-to ? action walk-to argument-1 ?place))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?place)))

(defrule drop-object-once-moved :forward :context mab
  (goal-is-to ?goal action move argument-1 ?name argument-2 ?place)
  (monkey ?monkey location ?place holding ?obj)
  (thing ?thing name ?name weight light)
  -->
  ((format t "Monkey drops the ~A.~%" ?name))
  (assert monkey ?monkey holding blank)
  (assert (thing ?thing location ?place on-top-of floor)
  (erase ?goal))

(defrule already-moved-object :forward :context mab
  (goal-is-to ?goal action move argument-1 ?obj argument-2 ?place)
  (thing ? name ?obj location ?place)
  -->
  (erase ?goal))

;;; Walk-to-place rules...

(defrule already-at-place :forward :context mab
  (goal-is-to ?goal action walk-to argument-1 ?place)
  (monkey ? location ?place)
  -->
  (erase ?goal))

(defrule get-on-floor-to-walk :forward :context mab
  (goal-is-to ? action walk-to argument-1 ?place)
  (monkey ? location ?loc on-top-of ?on)
  (test (and (not (eql ?loc ?place))
             (not (eql ?on 'floor))))
  (not (goal-is-to ? action on argument-1 floor))
  -->
  (assert (goal-is-to ? action on argument-1 floor)))

(defrule walk-holding-nothing :forward :context mab
  (goal-is-to ?goal action walk-to argument-1 ?place)
  (monkey ?monkey location ?loc on-top-of floor holding blank)
  (test (not (eql ?loc ?place)))
  -->
  ((format t "Monkey walks to ~A.~%" ?place))
  (assert (monkey ?monkey location ?place))
  (erase ?goal))

(defrule walk-holding-object :forward :context mab
  (goal-is-to ?goal action walk-to argument-1 ?place)
  (monkey ?monkey location ?loc on-top-of floor holding ?obj)
  (test (not (eql ?loc ?place)))
  (thing ? name ?obj)
  -->
  ((format t "Monkey walks to ~A holding the ~A.~%" ?place ?obj))
  (assert (monkey ?monkey location ?place))
  (erase ?goal))

;;; Get-on-object rules...

(defrule jump-onto-floor :forward :context mab
  (goal-is-to ?goal action on argument-1 floor)
  (monkey ?monkey on-top-of ?on)
  (test (not (eql ?on 'floor)))
  -->
  ((format t "Monkey jumps off the ~A onto the floor.~%" ?on))
  (assert (monkey ?monkey on-top-of floor))
  (erase ?goal))

(defrule walk-to-place-to-climb :forward :context mab
  (goal-is-to ? action on argument-1 ?obj)
  (thing ? name ?obj location ?place)
  (monkey ? location ?loc)
  (test (not (eql ?loc ?place)))
  (not (goal-is-to ? action walk-to argument-1 ?place))
  -->
  (assert (goal-is-to ? action walk-to argument-1 ?place)))

(defrule drop-to-climb :forward :context mab
  (goal-is-to ? action on argument-1 ?obj)
  (thing ? name ?obj location ?place)
  (monkey ? location ?place holding ?hold)
  (test (not (eql ?hold blank)))
  (not (goal-is-to ? action hold argument-1 blank))
  -->
  (assert (goal-is-to ? action hold argument-1 blank)))

(defrule climb-indirectly :forward :context mab
  (goal-is-to ? action on argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on)
  (monkey ? location ?place on-top-of ?top holding blank)
  (test (and (not (eql ?top ?on))
             (not (eql ?top ?obj))))
  (not (goal-is-to ? action on argument-1 ?on))
  -->
  (assert (goal-is-to ? action on argument-1 ?on)))

(defrule climb-directly :forward :context mab
  (goal-is-to ?goal action on argument-1 ?obj)
  (thing ? name ?obj location ?place on-top-of ?on)
  (monkey ?monkey location ?place on-top-of ?on holding blank)
  -->
  ((format t "Monkey climbs onto the ~A.~%" ?obj))
  (assert (monkey ?monkey on-top-of ?obj))
  (erase ?goal))

(defrule already-on-object :forward :context mab
  (goal-is-to ?goal action on argument-1 ?obj)
  (monkey ? on-top-of ?obj)
  -->
  (erase ?goal))

;;; Eat-object rules...

(defrule hold-to-eat :forward :context mab
  (goal-is-to ? action eat argument-1 ?obj)
  (monkey ? holding ?hold)
  (test (not (eql ?hold ?obj)))
  (not (goal-is-to ? action hold argument-1 ?obj))
  -->
  (assert (goal-is-to ? action hold argument-1 ?obj)))

(defrule satisfy-hunger :forward :context mab
  (goal-is-to ?goal action eat argument-1 ?name)
  (monkey ?monkey holding ?name)
  (think ?thing name ?name)
  =>
  ((format t "Monkey eats the ~A.~%" ?name))
  (assert (monkey ?monkey holding blank))
  (erase ?goal)
  (erase ?thing))

;;; startup rule...

(defun startup ()
  (assert (monkey ? location t5-7 on-top-of green-couch holding blank))
  (assert (thing ? name green-couch location t5-7 weight heavy
                 on-top-of floor))
  (assert (thing ? name red-couch location t2-2 
                 on-top-of floor weight heavy))
  (assert (thing ? name big-pillow location t2-2 
                 weight light on-top-of red-couch))
  (assert (thing (name red-chest) (location t2-2) 
                 (weight light) (on-top-of big-pillow)))
  (assert (chest (name red-chest) (contents ladder) (unlocked-by red-key)))
  (assert (thing (name blue-chest) (location t7-7) 
                 (weight light) (on-top-of ceiling)))
  (assert (thing (name grapes) (location t7-8) 
                 (weight light) (on-top-of ceiling)))
  (assert (chest (name blue-chest) (contents bananas) (unlocked-by blue-key)))
  (assert (thing (name blue-couch) (location t8-8) 
                 (on-top-of floor) (weight heavy)))
  (assert (thing (name green-chest) (location t8-8) 
                 (weight light) (on-top-of ceiling)))
  (assert (chest (name green-chest) (contents blue-key) (unlocked-by red-key)))
  (assert (thing (name red-key) 
                 (on-top-of floor) (weight light) (location t1-3)))
  (assert (goal-is-to (action eat) (argument-1 bananas) (argument-2 empty))))

#+ignore
(defun run-mab (&optional (ntimes 1))
  (let ((start (get-internal-real-time)))
    (dotimes (i ntimes)
      (format t "Starting run.~%")
      (reset)
      (run))
    (format t "Elapsed time: ~F~%"
            (/ (- (get-internal-real-time) start)
               internal-time-units-per-second))))

(defun run-mab (&optional (ntimes 1))
  (flet ((repeat-mab ()
           (dotimes (i ntimes)
             (format t "Starting run.~%")
             (reset)
             (run))))
    (time (repeat-mab))))

#+Allegro
(defun profile-mab (&optional (ntimes 10))
  (prof:with-profiling (:type :time)
    (run-mab ntimes)))

#+LispWorks
(defun profile-mab (&optional (ntimes 10))
  (hcl:set-up-profiler :packages "LISA")
  (hcl:profile (run-mab ntimes)))
