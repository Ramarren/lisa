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

;;; $Id: instrumenting.lisp,v 1.7 2001/03/02 21:50:50 youngde Exp $

(in-package :lisa)

(let ((instruments (make-hash-table)))
  (defun instrument (obj)
    (setf (gethash obj instruments) obj))

  (defun instrumentedp (obj)
    (gethash obj instruments))

  (defun uninstrument (obj)
    (remhash obj instruments)))

(defun mappaths (func rule-name)
  (mapc #'(lambda (path)
            (mapc func path))
        (find-paths-to-rule rule-name)))

(defun instrument-rule (rule-name)
  "Instruments each node in the network that leads to the rule
  identified by RULE-NAME."
  (mappaths #'instrument rule-name)
  (values))

(defun uninstrument-rule (rule-name)
  "Deactivates instrumenting of the rule identified by RULE-NAME."
  (mappaths #'uninstrument rule-name)
  (values))

(defun instrument-path (path)
  "Instruments each node in a particular activation path."
  (mapc #'instrument path)
  (values))

(defun uninstrument-path (path)
  "Deactivates instrumenting of an activation path."
  (mapc #'uninstrument path)
  (values))

(defun ibreak (obj &rest args)
  "Breaks if OBJ is instrumented."
  (when (instrumentedp obj)
    (apply #'break "Breaking on instrumented object" args)))
