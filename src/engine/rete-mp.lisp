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

;;; File: rete-mp.lisp
;;; Description: This class and its associated methods implement an MP-safe
;;; version of class RETE.

;;; $Id: rete-mp.lisp,v 1.1 2001/05/05 17:14:25 youngde Exp $

(in-package "LISA")

(defclass rete-mp (rete)
  ((lock :initform (lmp:make-lock :name "Rete Lock")
         :reader get-lock))
  (:documentation
   "This class and its associated methods implement an MP-safe version of
   class RETE."))

(defmethod assert-fact ((self rete-mp) (fact fact))
  (lmp:with-lock ((get-lock self))
    (call-next-method self fact)))

(defmethod retract-fact ((self rete-mp) (fact fact))
  (lmp:with-lock ((get-lock self))
    (call-next-method self fact)))

(defmethod modify-fact ((self rete-mp) fact slot-changes)
  (lmp:with-lock ((get-lock self))
    (call-next-method self fact slot-changes)))

(defmethod run-engine ((self rete-mp) &optional (step t))
  (lmp:with-lock ((get-lock self))
    (call-next-method self step)))
