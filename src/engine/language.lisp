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

;;; File: language.lisp
;;; Description: Code that implements the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.54 2001/09/10 18:04:58 youngde Exp $

(in-package "LISA")

(defmacro defrule (name (&key (salience 0) (module nil)) &body body)
  (let ((rule-name (gensym)))
    `(let ((,rule-name
            (if (consp ',name) ,name ',name)))
       (redefine-defrule ,rule-name ',body :salience ,salience :module ,module))))

(defmacro deftemplate (name (&key &allow-other-keys) &body body)
  `(redefine-deftemplate ',name ',body))

(defmacro deffacts (name (&key &allow-other-keys) &body body)
  (parse-and-insert-deffacts name body))

(defmacro defimport (symbolic-name (class-name 
                                    &optional (superclasses nil))
                     (&body body))
  (redefine-defimport symbolic-name class-name superclasses body))

(defmacro engine ()
  `(values ,+lisa-engine-var+))

(defmacro assert ((&body body))
  (parse-and-insert-fact body))

(defun assert-instance (instance)
  (parse-and-insert-instance instance))

(defun retract-instance (instance)
  (parse-and-retract-instance instance))

(defun assert-from-string (str)
  (eval (read-from-string str)))

(defun facts (&optional (engine (current-engine)))
  (print-fact-list engine))

(defun rules (&optional (engine (current-engine)))
  (print-rule-list engine))

(defun agenda (&optional (engine (current-engine)))
  (print-activation-list engine))

(defun reset (&optional (engine (current-engine)))
  (reset-engine engine))

(defun clear (&optional (engine (current-engine)))
  (clear-environment engine))

(defun run (&optional (engine (current-engine)) &key (step t))
  (run-engine engine step))

(defun walk (&optional (step 1))
  (run-engine (current-engine) step))

(defun retract (fact &optional (engine (current-engine)))
  (retract-fact engine fact))

(defmacro modify (fact &body body)
  (parse-and-modify-fact fact body))

(defun watch (event)
  (watch-event event))

(defun unwatch (event)
  (unwatch-event event))

(defun watching ()
  (format t "Watching: ~S~%" (get-watches)))

(defun halt (engine)
  (halt-engine engine))

(defun print-activation-list (engine)
  (let ((activations (get-activation-list engine)))
    (mapc #'(lambda (act)
              (format t "~S~%" act))
          activations)
    (format t "For a total of ~D activation~:P.~%"
            (length activations))
    (values)))

(defun print-rule-list (engine)
  (let ((rules (get-rule-list engine)))
    (mapc #'(lambda (rule)
              (format t "~S~%" rule))
          rules)
    (format t "For a total of ~D rule~:P.~%" (length rules))
    (values)))

(defun print-fact-list (engine)
  (let ((facts (get-fact-list engine)))
    (mapc #'(lambda (fact)
              (format t "~S~%" fact))
          facts)
    (format t "For a total of ~D fact~:P.~%" (length facts))
    (values)))
