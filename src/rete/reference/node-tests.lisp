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

;;; File: node-tests.lisp
;;; Description:

;;; $Id: node-tests.lisp,v 1.16 2002/09/27 20:51:00 youngde Exp $

(in-package "LISA")

(let ((*node-test-table*
       (make-hash-table :test #'equal)))

  (defun find-test (key constructor)
    (let ((test (gethash key *node-test-table*)))
      (when (null test)
        (setf test
          (setf (gethash key *node-test-table*)
            (funcall constructor))))
      test)))

(defun make-class-test (class)
  (find-test class
             #'(lambda ()
                 (function
                  (lambda (token)
                    (eq class (fact-name (token-top-fact token))))))))

(defun make-simple-slot-test-aux (slot-name value negated-p)
  (find-test 
   `(,slot-name ,value ,negated-p)
   #'(lambda ()
       (let ((test
              (function
               (lambda (token)
                 (equal value
                        (get-slot-value
                         (token-top-fact token)
                         slot-name))))))
         (if negated-p
             (complement test)
           test)))))

(defun make-simple-slot-test (slot)
  (declare (type pattern-slot slot))
  (make-simple-slot-test-aux
   (pattern-slot-name slot)
   (pattern-slot-value slot)
   (pattern-slot-negated slot)))

#+ignore
(defmacro make-variable-test (slot-name binding)
  `(function
    (lambda (tokens)
      (equal (get-slot-value (token-top-fact tokens) ,slot-name)
             (get-slot-value 
              (token-find-fact tokens (binding-address ,binding))
              (binding-slot-name ,binding))))))

(defun make-inter-pattern-test (slot)
  (let* ((binding (pattern-slot-slot-binding slot))
         (test
          (function
           (lambda (tokens)
             (equal (get-slot-value (token-top-fact tokens)
                                    (pattern-slot-name slot))
                    (get-slot-value
                     (token-find-fact tokens (binding-address binding))
                     (binding-slot-name binding)))))))
    (if (negated-slot-p slot) (complement test) test)))

(defun make-intra-pattern-test (slot)
  (let ((test
         (function
          (lambda (tokens)
            (equal (get-slot-value (token-top-fact tokens)
                                   (pattern-slot-name slot))
                   (get-slot-value (token-top-fact tokens)
                                   (binding-slot-name 
                                    (pattern-slot-slot-binding slot))))))))
    (if (negated-slot-p slot) (complement test) test)))

(defun make-predicate-test (forms bindings &optional (negated-p nil))
  (let* ((special-vars
          (mapcar #'binding-variable bindings))
         (body
          (if (consp (first forms)) 
              forms
            (list forms)))
         (predicate
          (compile nil `(lambda ()
                          (declare (special ,@special-vars))
                          ,@body)))
         (test
          (function
           (lambda (tokens)
             (handler-case
                 (progv
                     `(,@special-vars)
                     `(,@(mapcar #'(lambda (binding)
                                     (if (pattern-binding-p binding)
                                         (token-find-fact 
                                          tokens (binding-address binding))
                                       (get-slot-value
                                        (token-find-fact 
                                         tokens (binding-address binding))
                                        (binding-slot-name binding))))
                                 bindings))
                   (funcall predicate))
               (error (e)
                 (progn
                   (format t "forms: ~S~%" forms)
                   (format t "negated-p: ~S~%" negated-p)
                   (format t "bindings: ~S~%" bindings)
                   (format t "token: ~S~%" tokens)
                   (format t "token stack: ~S~%" (token-facts tokens))
                   (break))))))))
    (if negated-p (complement test) test)))

(defun make-intra-pattern-predicate-test (forms bindings 
                                          &optional (negated-p nil))
  (let* ((special-vars
          (mapcar #'binding-variable bindings))
         (body
          (if (consp (first forms)) 
              forms
            (list forms)))
         (predicate
          (compile nil `(lambda ()
                          (declare (special ,@special-vars))
                          ,@body)))
         (test
          (function
           (lambda (tokens)
             (handler-case
                 (progv
                     `(,@special-vars)
                     `(,@(mapcar #'(lambda (binding)
                                     (if (pattern-binding-p binding)
                                         (token-find-fact 
                                          tokens (binding-address binding))
                                       (get-slot-value
                                        (token-top-fact tokens)
                                        (binding-slot-name binding))))
                                 bindings))
                   (funcall predicate))
               (error (e)
                 (progn
                   (format t "forms: ~S~%" forms)
                   (format t "negated-p: ~S~%" negated-p)
                   (format t "bindings: ~S~%" bindings)
                   (format t "token: ~S~%" tokens)
                   (format t "token stack: ~S~%" (token-facts tokens))
                   (break))))))))
    (if negated-p (complement test) test)))
         
(defun make-behavior (function bindings)
  (make-predicate-test function bindings))

