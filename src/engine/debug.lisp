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

;;; File: debug.lisp
;;; Description: Utilities and functions useful for inspection and
;;; debugging of Lisa during development.

;;; $Id: debug.lisp,v 1.18 2001/08/23 23:53:27 youngde Exp $

(in-package "LISA")

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
  (let ((collection nil)
        (node-class (find-class node-type)))
    (labels ((trace-graph (nodes)
               (let ((node (first nodes)))
                 (cond ((null node)
                        (values nil))
                       (t
                        (when (eq (class-of node) node-class)
                          (push node collection))
                        (trace-graph (get-successors node))
                        (trace-graph (rest nodes)))))))
      (trace-graph (get-successors (get-root-node
                                    (get-compiler engine)))))
    (remove-duplicates collection :test #'eq)))

(defun find-node2 (&optional (engine (current-engine)))
  (find-node 'node2 engine))

(defun print-paths-to-rule (rule-name &optional (strm t))
  (labels ((print-path (nodes level)
             (unless (null nodes)
               (let ((obj (format nil "~S" (first nodes))))
                 (format strm "~V<~A~>~%"
                         (+ level (length obj)) obj)
                 (print-path (rest nodes) (+ level 3))))))
    (mapc #'(lambda (path)
              (print-path path 0))
          (find-paths-to-rule (current-engine) rule-name)))
  (values))

(defun find-object (addr)
  #+Lispworks
  (sys:pointer-from-address addr)
  #-Lispworks
  (declare (ignore addr))
  #-Lispworks
  (error "FIND-OBJECT not implemented for this platform."))

(defun show-token (token)
  (do ((tok token (get-parent tok)))
      ((null tok) t)
    (describe tok))
  (terpri))

(defun show-node2-memories (node2)
  (format t "Right memory for node ~S~%" node2)
  (with-tree-iterator (token (get-right-tree node2))
    (show-token token))
  (terpri)
  (format t "Left memory for node ~S~%" node2)
  (with-tree-iterator (token (get-left-tree node2))
    (show-token token))
  (values t))
  
