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

;;; File: rete-compiler.lisp
;;; Description:

;;; $Id: rete-compiler.lisp,v 1.2 2002/08/28 18:52:00 youngde Exp $

(in-package "LISA")

(defclass rete-network ()
  ((roots :initform
          (make-array nil :adjustable t :fill-pointer t)
          :reader rete-roots)))

(defun make-rete-network ()
  (make-instance 'rete-network))

(defun pass-token (node1 token)
  (accept-token node1 token))

(defun pass-tokens-on-left (node2 tokens)
  (accept-tokens-from-left node2 tokens))

(defun pass-token-on-right (node2 token)
  (accept-token-from-right node2 token))

(defun compile-rule-into-network (patterns))
