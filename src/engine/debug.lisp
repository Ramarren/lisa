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

;;; $Id: debug.lisp,v 1.10 2001/02/06 21:42:20 youngde Exp $

(in-package :lisa)

(defun trace-rete (&optional (engine (current-engine)))
  (let ((root-node (get-root-node (get-compiler engine))))
    (labels ((trace-nodes (nodes level)
               (let ((node (first nodes)))
                 (cond ((null node)
                        (values nil))
                       (t
                        (let ((obj (format nil "~S" node)))
                          (format t "~V<~A~>~%"
                                  (+ level (length obj)) obj)
                          (trace-nodes (get-successors node) (+ level 3))
                          (trace-nodes (rest nodes) level)))))))
      (format t "~S~%" root-node)
      (trace-nodes (get-successors root-node) 3))))

(defun write-rete (strm &optional (engine (current-engine)))
  (let ((root-node (get-root-node (get-compiler engine))))
    (labels ((trace-nodes (nodes level)
               (let ((node (first nodes)))
                 (cond ((null node)
                        (values nil))
                       (t
                        (let ((obj (format nil "~S" (class-name (class-of node)))))
                          (format strm "~V<~A~>~%"
                                  (+ level (length obj)) obj)
                          (trace-nodes (get-successors node) (+ level 3))
                          (trace-nodes (rest nodes) level)))))))
      (format strm "~S~%" (class-name (class-of root-node)))
      (trace-nodes (get-successors root-node) 3))))

(defun find-rule (name &optional (engine (current-engine)))
  (find-if #'(lambda (rule) (eq name (get-name rule)))
           (get-rule-list engine)))
  
(defun find-node (node-type &optional (engine (current-engine)))
  (let ((collection nil))
    (labels ((trace-graph (nodes)
               (let ((node (first nodes)))
                 (cond ((null node)
                        (values nil))
                       (t
                        (when (typep node node-type)
                          (push node collection))
                        (trace-graph (get-successors node))
                        (trace-graph (rest nodes)))))))
      (trace-graph (get-successors (get-root-node
                                    (get-compiler engine)))))
    (remove-duplicates collection :test #'eq)))

(defun find-node2 (&optional (engine (current-engine)))
  (find-node 'node2 engine))
