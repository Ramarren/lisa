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

;;; File: instrumenting.lisp
;;; Description: This file contains code used to instrument and
;;; analyse a Rete network. The idea is that this stuff can be used to
;;; help debug a malfunctioning complex network. We'll see...

;;; $Id: instrumenting.lisp,v 1.3 2001/02/09 01:49:59 youngde Exp $

(in-package :lisa)

(defun instrumentedp (node)
  (get-instrumenting node))

(defun maprule (func rule-name)
  (mapc #'(lambda (path)
            (mapc func path))
        (find-paths-to-rule rule-name)))

(defun instrument-rule (rule-name &key (type :minimal))
  "Instruments each node in the network that leads to the rule
  identified by RULE-NAME."
  (maprule #'(lambda (node)
               (setf (get-instrumenting node) type))
           rule-name)
  (values t))

(defun un-instrument-rule (rule-name)
  "Deactivates instrumenting of the rule identified by RULE-NAME."
  (maprule #'(lambda (node)
               (setf (get-instrumenting node) nil))
           rule-name)
  (values t))

(defmethod call-node-right :around ((self node) (token add-token))
  (let ((instrumented (get-instrumenting self)))
    (when instrumented
      (format t "call-node-right (~S)~%" self)
      (when (eq instrumented :verbose)
        (format t "  token is ~S~%" token)))
    (call-next-method self token)))

(defmethod call-node-left :around ((self node) (token add-token))
  (let ((instrumented (get-instrumenting self)))
    (when instrumented
      (format t "call-node-left (~S)~%" self))
    (call-next-method self token)))

(defmethod call-node-left :around ((self terminal-node) (token add-token))
  (when (instrumentedp self)
    (format t "Reaching activation for rule ~S, token ~S~%"
            (get-name (get-rule self)) token))
  (call-next-method self token))

(defmethod call-node-left :around ((self terminal-node) (token remove-token))
  (when (instrumentedp self)
    (format t "Removing activation for rule ~S, token ~S~%"
            (get-name (get-rule self)) token))
  (call-next-method self token))

(defparameter *node-list* nil)
(defparameter *token-list* nil)

(defmethod run-tests :around ((self node2) token &optional fact)
  (let ((rval (call-next-method self token fact))
        (instrumented (get-instrumenting self)))
    (when instrumented
      (format t "run-tests (~S), returning ~S~%" self rval)
      (when (eq instrumented :test-detail)
        (format t "  token is ~S~%" token)
        (format t "  fact is ~S~%" fact)
        (pushnew self *node-list*)
        (pushnew token *token-list*)))
    (values rval)))
