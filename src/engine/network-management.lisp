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

;;; File: network-management.lisp
;;; Description: Functions used to maintain and inspect the Rete network.

;;; $Id: network-management.lisp,v 1.4 2001/08/24 14:30:07 youngde Exp $

(in-package "LISA")

(defun find-paths-to-rule (engine rule-name)
  (let ((root (get-root-node (get-compiler engine)))
        (paths (list)))
    (labels ((trace-graph (nodes path)
               (let ((node (first nodes)))
                 (cond ((null node)
                        (values nil))
                       ((and (typep node 'terminal-node)
                             (eq rule-name (get-name (get-rule node))))
                        (push (append path `(,node)) paths)
                        (values t))
                       (t
                        (trace-graph (get-successors node)
                                     (append path `(,node)))
                        (trace-graph (rest nodes) path))))))
      (trace-graph (get-successors root) (list root)))
    (values paths)))

(defun remove-rule-from-network (engine rule-name)
  (labels ((remove-rule (root-node nodes)
             (cond ((null nodes) t)
                   (t
                    (let ((node (first nodes)))
                      (when (= 0 (decrease-use-count node))
                        (remove-successor root-node node))
                      (remove-rule node (rest nodes)))))))
    (let ((paths (find-paths-to-rule engine rule-name)))
      (cl:assert (not (null paths)) ()
        "No path(s) in the network found for rule ~A~%" rule-name)
      (mapc #'(lambda (path)
                (remove-rule (first path) (rest path)))
            paths))))
