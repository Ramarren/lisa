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

;;; File: token.lisp
;;; Description: A TOKEN is the fundamental unit of communication in
;;; the Rete network. TOKENs represent one or more facts, and
;;; subclasses of TOKEN represent network operations (eg. ADD,
;;; REMOVE).

;;; $Id: token.lisp,v 1.1 2000/11/03 21:36:51 youngde Exp $

(in-package "LISA")

(defclass token ()
  ((sort-code :initform 0
              :reader get-sort-code)
   (fact :initform nil
         :initarg :fact
         :reader get-fact)
   (parent :initform nil
           :accessor get-parent)
   (depth :initform 0
          :reader get-depth)
   (neg-count :initform 0
              :accessor get-neg-count)
   (clock :initform 0
          :accessor get-clock))
  (:documentation
   "A TOKEN is the fundamental unit of communication in the Rete
   network. TOKENs represent one or more facts, and subclasses of
   TOKEN represent network operations (eg. ADD, REMOVE)."))

(defmethod get-top-fact ((self token))
  (get-fact self))

(defmethod find-fact ((self token) level)
  (labels ((traverse (token level)
             (cond ((<= level 0)
                    (get-parent token))
                   (t
                    (traverse (get-parent token) (1- level))))))
    (traverse (- (get-depth self) level))))

(defmethod size ((self token))
  (get-depth self))

(defmethod update-time ((self token) (engine rete))
  (decf (get-clock self)
        (+ (get-time (get-top-fact self))
           (get-time engine))))

(defmethod equals ((self token) (tok token))
  (eql (get-fact-id (get-fact self))
       (get-fact-id (get-fact tok))))

;;; various constructors for making TOKEN instances...

(defmethod make-token ((top-fact fact))
  (make-instance 'token
    :fact fact
    :depth 1
    :clock (get-time top-fact)
    :sort-code (get-fact-id top-fact)))

(defmethod make-token ((tok token) (new-fact fact))
  (make-instance 'token
    :fact new-fact
    :parent tok
    :depth (1+ (get-depth tok))
    :sort-code (+ (ash (get-sort-code tok) 3)
                  (get-fact-id new-fact))
    :clock (+ (get-time new-fact) (get-clock tok))))

(defmethod make-token ((left token) (right token))
  (make-token left (get-top-fact right)))

(defmethod make-token ((tok token))
  (make-instance 'token
    :fact (get-top-fact tok)
    :parent (get-parent tok)
    :depth (get-depth tok)
    :sort-code (get-sort-code tok)
    :clock (get-clock tok)
    :neg-count (get-neg-count tok)))

