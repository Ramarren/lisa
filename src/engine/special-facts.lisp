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

;;; File: special-facts.lisp
;;; Description: Classes that represents special facts internal to
;;; LISA.

;;; $Id: special-facts.lisp,v 1.2 2000/11/27 16:22:50 youngde Exp $

(in-package :lisa)

(defclass initial-fact (lisa-kb-class)
  ()
  (:documentation
   "Represents the special fact INITIAL-FACT."))

(defclass clear-fact (lisa-kb-class)
  ()
  (:documentation
   "Represents the special fact CLEAR-FACT."))

(defclass not-or-test-fact (lisa-kb-class)
  ()
  (:documentation
   "Represents the special fact NOT-OR-TEST."))

