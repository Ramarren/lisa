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

;;; $Id: instrumenting.lisp,v 1.2 2001/02/08 17:35:08 youngde Exp $

(in-package :lisa)

(defmacro symbol-of (instance)
  `(class-name (class-of ,instance)))

(defmacro instrumentedp (node)
  `(get (class-name (class-of ,node)) 'instrumented))

(defun maprule (func rule-name)
  (mapc #'(lambda (path)
            (mapc #'func path))
        (find-paths-to-rule rule-name)))

(defun instrument-rule (rule-name)
  "Instruments each node in the network that leads to the rule
  identified by RULE-NAME."
  (maprule #'(lambda (node)
               (setf (get (symbol-of node) 'instrumented) t))
           rule-name))

(defun un-instrument-rule (rule-name)
  "Deactivates instrumenting of the rule identified by RULE-NAME."
  (maprule #'(lambda (node)
               (remprop (symbol-of node) 'instrumented))
           rule-name))

(defmethod call-node-right :around ((self node2) (token add-token))
  (let ((rval (call-next-method self token)))
    (when (instrumentedp self)
      (format t "call-node-right for (~S, ~S) returning ~S~%"
              self token rval))
    (values rval)))

(defmethod call-node-left :around ((self node2) (token add-token))
  (let ((rval (call-next-method self token)))
    (when (instrumentedp self)           
      (format t "call-node-left for (~S, ~S) returning ~S~%"
              self token rval))
    (values rval)))

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
