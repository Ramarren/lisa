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

;;; File: dynamic-update.lisp
;;; Description: This file contains code supporting LISA's dynamic update
;;; capabilities.

;;; $Id: dynamic-update.lisp,v 1.3 2001/08/28 16:02:30 youngde Exp $

(in-package "LISA")

(defvar *in-dynamic-update* nil
  "Set whenever a dynamic update is in progress.")

(defmacro with-gatekeeper ((node) &body body)
  "The gatekeeper is a mechanism Rete nodes use to successfully participate in
  the dynamic update protocol. The code body for each network node's entry
  point(s) to the pattern matching process (i.e. CALL-NODE-RIGHT,
  CALL-NODE-LEFT) should be wrapped in this macro unless the node does nothing
  during this time."
  (let ((pass-the-gate (gensym)))
    `(labels ((,pass-the-gate ()
                ,@body))
       (cond (*in-dynamic-update*
              (if (of-dynamic-update ,node)
                  (funcall #',pass-the-gate)
                (values nil)))
             (t (funcall #',pass-the-gate))))))

(defmacro with-dynamic-update ((engine rule) &body body)
  (let ((toggle-dynamic-update (gensym))
        (paths (gensym)))
    `(labels ((,toggle-dynamic-update (on paths)
                (dolist (path paths)
                  (mapc #'(lambda (node)
                            (setf (get-dynamic-update node) on))
                        path))))
       (let ((*in-dynamic-update* t)
             (,paths (find-paths-to-rule ,engine (get-name ,rule))))
         (cl:assert (not (null ,paths)) ()
           "No paths in the network to this rule: ~A~%" (get-name ,rule))
         (funcall #',toggle-dynamic-update t ,paths)
         (unwind-protect
             (progn ,@body)
           (funcall #',toggle-dynamic-update nil ,paths))))))
     
