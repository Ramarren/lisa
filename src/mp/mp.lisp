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

;;; File: mp.lisp
;;; Description: Interfaces to the CLOCC multiprocessing library. If the Lisp
;;; implementation does not support MP, then the interfaces are either no-ops
;;; or signal an error. The idea here is that core LISA can transparently
;;; support some "virtual" MP functionality (particularly mutexes) regardless
;;; of the underlying Lisp platform. For example, LISA can safely invoke
;;; things like WITH-LOCK without considering whether or not Lisp MP support
;;; is available. Of course, this does NOT hold for functions like
;;; MAKE-PROCESS; in cases like this we defer to the underlying behavior of
;;; CLOCC.

;;; NB: This stuff is NOT intended for consumption outside of core LISA.

;;; $Id: mp.lisp,v 1.2 2001/04/26 17:12:57 youngde Exp $

(in-package "LISA.MULTIPROCESSING")

#+threads
(defmacro make-lock (&rest args)
  `(apply #'port:make-lock ,args))

#-threads
(defmacro make-lock (&rest args)
  (declare (ignore args)))

#+threads
(defmacro get-lock (lock)
  `(port:get-lock ,lock))

#-threads
(defmacro get-lock (lock)
  (declare (ignore lock)))

#+threads
(defmacro giveup-lock (lock)
  `(port:giveup-lock ,lock))

#-threads
(defmacro giveup-lock (lock)
  (declare (ignore lock)))

#+threads
(defmacro with-lock ((lock) &rest args)
  `(port:with-lock (,lock) ,args))

#-threads
(defmacro with-lock ((lock) &body body)
  (declare (ignore lock))
  `(progn ,@body))

#+threads
(defmacro without-scheduling (&rest args)
  `(port:without-scheduling ,args))

#-threads
(defmacro without-scheduling (&body body)
  `(progn ,@body))

