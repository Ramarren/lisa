;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

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
;;; $Id: language.lisp,v 1.29 2004/09/16 15:35:46 youngde Exp $

(in-package :lisa)

(defmacro defrule (name (&key (salience 0) 
                              (context nil)
                              (cf nil)
                              (auto-focus nil))
                   &body body)
  (let ((rule-name (gensym)))
    `(let ((,rule-name ,@(if (consp name)
                             `(,name)
                             `(',name))))
       (redefine-defrule ,rule-name
                         ',body
                         :salience ,salience
                         :context ,context
                         :cf ,cf
                         :auto-focus ,auto-focus))))

(defun undefrule (rule-name)
  (with-rule-name-parts (context short-name long-name) rule-name
    (forget-rule (inference-engine) long-name)))

(defmacro deftemplate (name (&key) &body body)
  (redefine-deftemplate name body))

(defmacro defimport (class-name)
  `(import-class-specification ',class-name))

(defmacro defcontext (context-name &optional (strategy nil))
  `(unless (find-context (inference-engine) ,context-name nil)
     (register-new-context (inference-engine) 
                           (make-context ,context-name :strategy ,strategy))))

(defmacro undefcontext (context-name)
  `(forget-context (inference-engine) ,context-name))

(defun focus-stack ()
  (rete-focus-stack (inference-engine)))

(defun focus (&rest args)
  (if (null args)
      (current-context (inference-engine))
    (dolist (context-name (reverse args) (focus-stack))
      (push-context
       (inference-engine) 
       (find-context (inference-engine) context-name)))))

(defun refocus ()
  (pop-context (inference-engine)))

(defun contexts ()
  (let ((contexts (retrieve-contexts (inference-engine))))
    (dolist (context contexts)
      (format t "~S~%" context))
    (format t "For a total of ~D context~:P.~%" (length contexts))
    (values)))

(defun dependencies ()
  (maphash #'(lambda (dependent-fact dependencies)
               (format *trace-output* "~S:~%" dependent-fact)
               (format *trace-output* "  ~S~%" dependencies))
           (rete-dependency-table (inference-engine)))
  (values))

(defun expand-slots (body)
  (mapcar #'(lambda (pair)
              (destructuring-bind (name value) pair
                `(list (identity ',name) 
                       (identity 
                        ,@(if (quotablep value)
                              `(',value)
                            `(,value))))))
          body))

#+ignore
(defun determine-cf (sample)
  (cl:assert (or (null sample) (cf:cf-p sample)) nil
    "This is not a legal certainty factor: ~S" sample)
  (cond (sample sample)
        ((in-rule-firing-p)
         (cf (active-rule)))
        nil))

(defun determine-cf (a)
  (cl:assert (or (null a) (cf:cf-p a)) nil
    "This is not a legal certainty factor: ~S" a)
  (if a
      (let ((b (apply #'min (loop for f across (token-facts *active-tokens*)
                                  if (plusp (cf f))
                                  collect (cf f)))))
        (if b
            (cf:combine a b)
          a))
    nil))

(defgeneric calculate-cf (fact-cf)
  (:method ((fact-cf number))
   fact-cf)
  (:method ((fact-cf t))
   (unless (in-rule-firing-p)
     (return-from calculate-cf nil))
   (let ((conjucts (loop for fact across (token-facts *active-tokens*)
                         if (plusp (cf fact))
                         collect (cf fact))))
     (cond (conjuncts
            (if (= (length conjuncts) 1)
                (first conjuncts)
              (apply #'min conjuncts)))
           (t nil)))))
         
(defmacro assert ((name &body body) &key (cf nil))
  (let ((fact (gensym))
        (fact-object (gensym))
        (actual-cf (gensym)))
    `(let ((,fact-object 
            ,@(if (or (consp name)
                      (variablep name))
                  `(,name)
                `(',name)))
           (,actual-cf (cf:determine-cf ,cf)))
       (if (typep ,fact-object 'standard-object)
           (parse-and-insert-instance ,fact-object :cf ,actual-cf)
         (progn
           (ensure-meta-data-exists ',name)
           (let ((,fact (make-fact ',name ,@(expand-slots body))))
             (when (and (in-rule-firing-p)
                        (logical-rule-p (active-rule)))
               (bind-logical-dependencies ,fact))
             (assert-fact (inference-engine) ,fact :cf ,actual-cf)))))))

(defmacro deffacts (name (&key &allow-other-keys) &body body)
  (parse-and-insert-deffacts name body))

(defun engine ()
  (active-engine))

(defun rule ()
  (active-rule))

(defun assert-instance (instance)
  (parse-and-insert-instance instance))

(defun retract-instance (instance)
  (parse-and-retract-instance instance (inference-engine)))

(defun facts ()
  (let ((facts (get-fact-list (inference-engine))))
    (dolist (fact facts)
      (format t "~S~%" fact))
    (format t "For a total of ~D fact~:P.~%" (length facts))
    (values)))

(defun rules (&optional (context-name nil))
  (let ((rules (get-rule-list (inference-engine) context-name)))
    (dolist (rule rules)
      (format t "~S~%" rule))
    (format t "For a total of ~D rule~:P.~%" (length rules))
    (values)))

(defun agenda (&optional (context-name nil))
  (let ((activations 
         (get-activation-list (inference-engine) context-name)))
    (dolist (activation activations)
      (format t "~S~%" activation))
    (format t "For a total of ~D activation~:P.~%" (length activations))
    (values)))

(defun reset ()
  (reset-engine (inference-engine)))

(defun clear ()
  (clear-system-environment))

(defun run (&optional (contexts nil))
  (unless (null contexts)
    (apply #'focus contexts))
  (run-engine (inference-engine)))

(defun walk (&optional (step 1))
  (run-engine (inference-engine) step))

(defmethod retract ((fact-object fact))
  (retract-fact (inference-engine) fact-object))

(defmethod retract ((fact-object number))
  (retract-fact (inference-engine) fact-object))

(defmethod retract ((fact-object t))
  (parse-and-retract-instance fact-object (inference-engine)))

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

(defun halt ()
  (halt-engine (inference-engine)))

(defun mark-instance-as-changed (instance &key (slot-id nil)) 
  (mark-clos-instance-as-changed (inference-engine) instance slot-id))
