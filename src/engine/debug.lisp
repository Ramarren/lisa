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

;;; File: debug.lisp
;;; Description: Utilities and functions useful for inspection and
;;; debugging of Lisa during development.

;;; $Id: debug.lisp,v 1.3 2000/11/13 19:27:05 youngde Exp $

(in-package :lisa)

(defun trace-rete (root-node)
  (labels ((trace-nodes (nodes level)
             (let ((node (first nodes)))
               (cond ((null node)
                      (values nil))
                     (t
                      (format t "~,,V<~S~>~%" level node)
                      (trace-nodes (get-successors node) (+ level 3))
                      (trace-nodes (rest nodes) level))))))
    (format t "root-node: ~S~%" root-node)
    (trace-nodes (get-successors root-node) 3)))

