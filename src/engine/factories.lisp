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

;;; File: factories.lisp
;;; Description: Factory code responsible for creating various types
;;; of LISA entities.

;;; $Id: factories.lisp,v 1.4 2000/11/17 02:52:29 youngde Exp $

(in-package "LISA")

(defun make-ce (pattern)
  (let ((tag (first pattern)))
    (cond ((eq tag 'test)
           (make-test-ce pattern))
          ((eq tag 'not)
           (make-not-ce pattern))
          ((symbolp tag)
           (make-and-ce pattern))
          (t (error "make-pattern: unrecognized CE type: ~S~%" pattern)))))

(defun make-pattern (head body)
  (make-generic-pattern head body))
