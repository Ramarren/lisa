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

;;; File: node2-not.lisp
;;; Description: Specialized two-input node for negated patterns.

;;; $Id: node2-not.lisp,v 1.3 2000/12/11 21:29:23 youngde Exp $

(in-package :lisa)

(defclass node2-not (node2)
  ()
  (:documentation
   "Represents a specialized two-input node for negated patterns."))

(defmethod call-node-left ((self node2-not) (token add-token))
  (call-next-method self (make-clone-token (class-of token) token)))

(defmethod run-tests-vary-right ((self node2-not) left-token tree)
  (with-tree-iterator (key right-token tree)
    (when (or (not (has-tests-p self))
              (run-tests self left-token (get-top-fact right-token)))
      (increment-negation-count left-token)))
  (unless (is-negated-p left-token)
    (with-accessors ((engine get-engine)) self
      (let ((token (make-derived-token
                    (class-of left-token) (get-null-fact engine))))
        (update-time token engine self)
        (increment-matches self left-token)
        (pass-along self token))))
  (values nil))

(defmethod pass-new-token ((self node2-not) (right-token add-token) left-token)
  (with-accessors ((engine get-engine)) self
    (let ((token (make-remove-token :parent left-token
                                    :initial-fact (get-null-fact engine))))
      (update-time token self)
      (pass-along self token)
      (increment-negation-count token)))
  (values nil))

(defmethod pass-new-token ((self node2-not) (right-token clear-token) left-token)
  (when (= (decrement-negation-count left-token) 0)
    (with-accessors ((engine get-engine)) self
      (let ((token (make-derived-token (class-of left-token)
                                       (get-null-fact engine))))
        (update-time token engine)
        (pass-along self token))))
  (values))

(defmethod run-tests-vary-left ((self node2-not) right-token tree)
  (with-tree-iterator (key left-token tree)
    (when (or (not (has-tests-p self))
              (run-tests self left-token (get-top-fact right-token)))
      (pass-new-token self right-token left-token)))
  (values nil))
