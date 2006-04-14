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

;;; File: conditions.lisp
;;; Description:

;;; $Id: conditions.lisp,v 1.1 2006/04/14 16:44:37 youngde Exp $

(in-package :lisa)

(define-condition duplicate-fact (error)
  ((existing-fact :reader duplicate-fact-existing-fact
                  :initarg :existing-fact))
  (:report (lambda (condition strm)
             (format strm "Lisa detected an attempt to assert a duplicate for: ~S"
                     (duplicate-fact-existing-fact condition)))))
                  
(define-condition parsing-error (error)
  ((text :initarg :text
         :initform nil
         :reader text))
  (:report (lambda (condition strm)
             (format strm "Parsing error: ~A" (text condition)))))

(define-condition class-parsing-error (parsing-error)
  ((class-name :initarg :class-name
               :initform nil
               :reader class-name))
  (:report (lambda (condition strm)
             (format strm "Class parsing error: ~A, ~A" (class-name condition) (text condition)))))

(define-condition rule-parsing-error (parsing-error)
  ((rule-name :initarg :rule-name
              :initform nil
              :reader rule-name)
   (pattern-location :initarg :pattern-location
                     :initform nil
                     :reader pattern-location))
  (:report (lambda (condition strm)
             (format strm "Rule parsing error: rule name ~A, pattern location ~A"
                     (rule-name condition) (pattern-location condition)))))