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

;;; File: add-token.lisp
;;; Description: Represents TOKENs used for add operations on the
;;; network.

;;; $Id: add-token.lisp,v 1.11 2001/03/26 16:27:23 youngde Exp $

(in-package "LISA")

(defclass add-token (token)
  ()
  (:documentation
   "Represents TOKENs used for add operations on the network."))

(defmethod print-object ((self add-token) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "(class = ~S)"
            (get-class (get-top-fact self)))))

(defun make-add-token (&rest args)
  (apply #'make-token (find-class 'add-token) args))


