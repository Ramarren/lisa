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
;;; $Id: language.lisp,v 1.13 2002/11/15 16:36:04 youngde Exp $

(in-package "LISA")

(defmacro defrule (name (&key (salience 0) (module nil)) &body body)
  (let ((rule-name (gensym)))
    `(let ((,rule-name ,@(if (consp name)
                             `(,name)
                             `(',name))))
       (redefine-defrule ,rule-name
                         ',body
                         :salience ,salience
                         :module ,module))))

(defmacro deftemplate (name (&key) &body body)
  (redefine-deftemplate name body))

(defmacro defimport (class-name &key (use-inheritancep t))
  (declare (ignore use-inheritancep))
  `(import-class-specification ',class-name))

(defun expand-slots (body)
  (mapcar #'(lambda (pair)
              (destructuring-bind (name value) pair
                `(list (identity ',name) 
                       (identity 
                        ,@(if (quotablep value)
                              `(',value)
                            `(,value))))))
          body))

(defmacro assert ((name &body body))
  (let ((fact (gensym))
        (fact-object (gensym)))
    `(let ((,fact-object 
            ,@(if (or (consp name)
                      (variablep name))
                  `(,name)
                `(',name))))
       (if (typep ,fact-object 'standard-object)
           (parse-and-insert-instance ,fact-object)
         (progn
           (ensure-meta-data-exists ',name)
           (let ((,fact (make-fact ',name ,@(expand-slots body))))
             (when (and (in-rule-firing-p)
                        (logical-rule-p (active-rule)))
               (bind-logical-dependencies ,fact))
             (assert-fact (inference-engine) ,fact)))))))

(defmacro deffacts (name (&key &allow-other-keys) &body body)
  (parse-and-insert-deffacts name body))

(defun engine ()
  (active-engine))

(defun rule ()
  (active-rule))

(defun assert-instance (instance)
  (warn "ASSERT-INSTANCE is deprecated. Use ASSERT instead.")
  (parse-and-insert-instance instance))

(defun retract-instance (instance)
  (warn "RETRACT-INSTANCE is deprecated. Use RETRACT instead.")
  (parse-and-retract-instance instance (inference-engine)))

(defun assert-from-string (str)
  (eval (read-from-string str)))

(defun facts (&optional (engine *active-engine*))
  (print-fact-list engine))

(defun rules (&optional (engine *active-engine*))
  (print-rule-list engine))

(defun agenda (&optional (engine *active-engine*))
  (print-activation-list engine))

(defun reset (&optional (engine *active-engine*))
  (reset-engine engine))

(defun clear ()
  (clear-system-environment))

(defun run (&optional (engine *active-engine*))
  (run-engine engine))

(defun walk (&optional (engine *active-engine*) (step 1))
  (run-engine engine step))

(defmethod retract ((fact-object fact) &optional (engine *active-engine*))
  (retract-fact engine fact-object))

(defmethod retract ((fact-object number) &optional (engine *active-engine*))
  (retract-fact engine fact-object))

(defmethod retract ((fact-object t) &optional (engine *active-engine*))
  (parse-and-retract-instance fact-object engine))

(defmacro modify (fact &body body)
  `(modify-fact (inference-engine) ,fact ,@(expand-slots body)))

(defun watch (event)
  (watch-event event))

(defun unwatch (event)
  (unwatch-event event))

(defun watching ()
  (let ((watches (watches)))
    (format *trace-output* "Watching ~A~%"
            (if watches watches "nothing"))
    (values)))

(defun halt (&optional (engine *active-engine*))
  (halt-engine engine))

(defun mark-instance-as-changed (instance &key (engine *active-engine*)
                                               (slot-id nil)) 
  (mark-clos-instance-as-changed engine instance slot-id))

(defun print-activation-list (engine)
  (let ((activations (make-activation-list engine)))
    (mapc #'(lambda (act)
              (format t "~S~%" act))
          activations)
    (format t "For a total of ~D activation~:P.~%"
            (length activations))
    (values)))

(defun print-rule-list (engine)
  (let ((rules (make-rule-list engine)))
    (mapc #'(lambda (rule)
              (format t "~S~%" rule))
          rules)
    (format t "For a total of ~D rule~:P.~%" (length rules))
    (values)))

(defun print-fact-list (engine)
  (let ((facts (make-fact-list engine)))
    (mapc #'(lambda (fact)
              (format t "~S~%" fact))
          facts)
    (format t "For a total of ~D fact~:P.~%" (length facts))
    (values)))

