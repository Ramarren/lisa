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

;;; File: test-facts.lisp
;;; Description: Some simple classes useful in early Lisa testing.

;;; $Id: test-facts.lisp,v 1.1 2000/11/09 20:41:47 youngde Exp $

(in-package :lisa)

(defclass rocky ()
  ((name :initarg :name
         :initform nil
         :reader get-name)))

(defclass boris ()
  ((name :initarg :name
         :initform nil
         :reader get-name)))

(defun make-rocky (name)
  (make-instance 'rocky :name name))

(defun make-boris (name)
  (make-instance 'boris :name name))

  
  
  
