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

;;; File: remove-token.lisp
;;; Description: Represents tokens used for remove operations on the
;;; network.

;;; $Id: remove-token.lisp,v 1.1 2000/11/19 21:13:09 youngde Exp $

(in-package :lisa)

(defclass remove-token (token)
  ()
  (:documentation
   "Represents tokens used for remove operations on the network."))

(defun make-remove-token (&rest args)
  (apply #'make-token (find-class 'remove-token) args))

