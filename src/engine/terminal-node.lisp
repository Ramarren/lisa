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

;;; File: terminal-node.lisp
;;; Description: Represents terminal nodes in the Rete network.

;;; $Id: terminal-node.lisp,v 1.13 2000/12/16 02:16:52 youngde Exp $

(in-package :lisa)

(defclass terminal-node (node)
  ((rule :initarg :rule
         :initform nil
         :reader get-rule))
  (:documentation
   "Represents terminal nodes in the Rete network."))

(defmethod call-node-left ((self terminal-node) (token add-token))
  (with-accessors ((rule get-rule)) self
    (create-activation (get-engine rule) rule token)
    (values t)))

(defmethod call-node-left ((self terminal-node) (token clear-token))
  (values t))

(defmethod call-node-left ((self terminal-node) (token remove-token))
  (let* ((engine (get-engine (get-rule self)))n
         (activation (find-activation engine token)))
    (unless (null activation)
      (disable-activation engine activation))
    (values t)))

(defmethod print-object ((self terminal-node) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(rule = ~S)" (get-name (get-rule self)))))

(defun make-terminal-node (rule)
  (make-instance 'terminal-node :rule rule))

