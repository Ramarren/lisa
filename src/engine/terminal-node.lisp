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

;;; $Id: terminal-node.lisp,v 1.9 2000/11/29 01:07:45 youngde Exp $

(in-package :lisa)

(defclass terminal-node (node)
  ((rule :initarg :rule
         :initform nil
         :reader get-rule))
  (:documentation
   "Represents terminal nodes in the Rete network."))

(defmethod call-node-left ((self terminal-node) (token add-token))
  (with-accessors ((rule get-rule)) self
    (format t "terminal-node: adding rule ~S to activation (~D).~%"
            (get-name rule) (hash-code token))
    (create-activation (get-engine rule) rule token)
    (values t)))

(defmethod call-node-left ((self terminal-node) (token clear-token))
  (format t "~S: received CLEAR token.~%" (class-name (class-of self)))
  (values t))

(defmethod call-node-left ((self terminal-node) (token remove-token))
  (format t "~S: removing activation (~D).~%" (class-name (class-of self))
          (hash-code token))
  (destroy-activation (get-engine (get-rule self)) token)
  (values t))

(defmethod print-object ((self terminal-node) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (format strm "(rule = ~S)" (get-name (get-rule self)))))

(defun make-terminal-node (rule)
  (make-instance 'terminal-node :rule rule))

