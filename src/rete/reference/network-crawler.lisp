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

;;; File: network-crawler.lisp
;;; Description:

;;; $Id: network-crawler.lisp,v 1.1 2002/09/03 19:51:47 youngde Exp $

(in-package "LISA")

(defun crawl-network (rete-network &optional (strm *terminal-io*))
  (labels ((get-roots ()
             (loop for node being the hash-value of (rete-roots rete-network)
                 collect node))
           (get-successors (shared-node)
             (loop for s being the hash-value 
                 of (shared-node-successors shared-node) 
                 collect (successor-node s)))
           (get-successor (join-node)
             (list (successor-node (node2-successor join-node))))
           (trace-nodes (nodes &optional (level 0))
             (unless (null nodes)
               (let* ((node (first node))
                      (string (format nil "~S" node)))
                 (format strm "~V<~A>~%" (+ level (length string)) string)
                 (if (typep node 'shared-node)
                     (trace-nodes (get-successors node) (+ level 3))
                   (trace-nodes (get-successor node) (+ level 3)))))))
    (trace-nodes (get-roots))))
