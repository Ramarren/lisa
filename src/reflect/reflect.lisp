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

;;; File: reflect.lisp
;;; Description: Wrapper functions that provide the MOP functionality needed
;;; by LISA, hiding implementation details.

;;; $Id: reflect.lisp,v 1.9 2002/06/06 21:28:34 youngde Exp $

(in-package "LISA.REFLECT")

#+(or CLISP CMU)
(defun ensure-class (name &key (direct-superclasses '()))
  (eval `(defclass ,name ,direct-superclasses ())))

#+CLISP
(defun class-finalized-p (class)
  (declare (ignore class))
  (values t))

#+CLISP
(defun finalize-inheritance (class)
  (declare (ignore class))
  (values))

(defun class-slot-list (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (port:class-slot-list class))

(defun is-standard-classp (class)
  (or (eq (class-name class) 'standard-object)
       (eq (class-name class) t)))

#+CLISP
(defun find-direct-superclasses (class)
  (remove-if #'is-standard-classp (clos::class-direct-superclasses class)))
             
#-CLISP
(defun find-direct-superclasses (class)
  (remove-if #'is-standard-classp (clos:class-direct-superclasses class)))
             
(defun class-all-superclasses (class-or-symbol)
  (labels ((find-superclasses (class-list superclass-list)
             (let ((class (first class-list)))
               (if (or (null class-list)
                       (is-standard-classp class))
                   superclass-list
                 (find-superclasses 
                  (class-direct-superclasses class)
                  (find-superclasses 
                   (rest class-list) (pushnew class superclass-list)))))))
    (let ((class
           (if (symbolp class-or-symbol)
               (find-class class-or-symbol)
             class-or-symbol)))
      (nreverse (find-superclasses (class-direct-superclasses class) nil)))))
