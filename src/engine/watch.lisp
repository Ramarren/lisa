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

;;; File: watch.lisp
;;; Description: Methods used to monitor various activities and events as they
;;; occur within Lisa. NB: This is throw-away code; eventually a more generic
;;; method for system monitoring will be developed, enabling observation from
;;; outside entities.

;;; $Id: watch.lisp,v 1.7 2001/02/03 21:55:22 youngde Exp $

(in-package :lisa)

(defparameter *watch-list* nil)

(defmacro watching-p (event)
  `(member ,event *watch-list*))

(defun watch-event (event)
  (pushnew event *watch-list*))

(defun unwatch-event (event)
  (setf *watch-list*
    (remove event *watch-list*)))

(defun get-watches ()
  (values *watch-list*))

(defun activation-fact-list (activation)
  (mapcar #'(lambda (fact)
              (get-symbolic-id fact))
          (remove-if #'(lambda (fact)
                         (= (get-fact-id fact) -1))
                     (get-all-facts (get-token activation)))))
  
(defun show-activation (direction activation)
  (format t "~A Activation: ~S : ~S~%"
          direction
          (get-name (get-rule activation))
          (activation-fact-list activation)))
  
(defmethod add-activation :before ((s strategy) (act activation))
  (when (watching-p :activations)
    (show-activation "==>" act))
  (values))

(defun show-activation-maybe (activation)
  (when (watching-p :activations)
    (show-activation "<==" activation))
  (values))

(defmethod disable-activation :before ((engine rete) (act activation))
  (show-activation-maybe act))

(defmethod fire-rule :before ((act activation))
  (when (watching-p :rules)
    (format t "FIRE ~D: ~S ~S~%"
            (get-fired-rule-count (get-engine (get-rule act)))
            (get-name (get-rule act)) (activation-fact-list act))))

(defun show-fact-detail (direction fact)
  (format t "~A f-~D ~S~%" direction (get-fact-id fact)
          (reconstruct-fact fact)))

(defmethod insert-token :before ((engine rete) (token add-token))
  (when (watching-p :facts)
    (show-fact-detail "==>" (get-top-fact token))))

(defmethod insert-token :before ((engine rete) (token remove-token))
  (when (watching-p :facts)
    (show-fact-detail "<==" (get-top-fact token))))
