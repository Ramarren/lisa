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

;;; File: node2-not.lisp
;;; Description:

;;; $Id: node2-not.lisp,v 1.2 2002/09/07 00:20:53 youngde Exp $

(in-package "LISA")

(defclass node2-not (join-node) ())

(defmethod test-and-pass-tokens ((self node2-not) left-tokens right-token)
  (let ((tests (join-node-tests self)))
    (token-push-fact left-tokens (token-top-fact right-token))
    (if (and (not (null tests))
             (notevery #'(lambda (test)
                           (funcall test left-tokens)) tests))
        (call-successor (join-node-successor self) left-tokens)
      (token-pop-fact left-tokens))))

(defun make-node2-not ()
  (make-instance 'node2-not))
