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

;;; File: activation.lisp
;;; Description: This class represents an activation of a rule.

;;; $Id: activation.lisp,v 1.1 2000/11/15 19:44:46 youngde Exp $

(in-package :lisa)

(defclass activation ()
  ((rule :initarg :rule
         :initform nil
         :reader get-rule)
   (token :initarg :token
          :initform nil
          :reader get-token))
  (:documentation
   "Represents a rule activation."))

(defmethod fire-rule ((self activation))
  (fire (get-rule self) (get-token self)))

(defun make-activation (rule token)
  (make-instance 'activation :rule rule :token token))

