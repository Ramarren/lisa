;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young

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

;;; File: mycin.lisp
;;; Description: An implementation of MYCIN as illustrated in PAIP, pg. 553. This example
;;; is used to exercise Lisa's new support for certainty factors.

;;; $Id: mycin.lisp,v 1.1 2004/09/15 18:34:06 youngde Exp $

(in-package :lisa-user)

(defclass culture ()
  ((site :initarg :site
         :initform nil
         :reader site)
   (days-old :initarg :days-old
             :initform nil
             :reader days-old)))

(defclass patient ()
  ((name :initarg :name
         :initform nil
         :reader name)
   (sex :initarg :sex
        :initform nil
        :reader sex)
   (age :initarg :age
        :initform nil
        :reader age)
   (burn :initarg :burn
         :initform nil
         :reader burn)
   (compromised-host :initarg :compromised-host
                     :initform nil
                     :reader compromised-host)))

(defclass organism ()
  ((identity :initarg :identity
             :initform :unknown
             :reader id)
   (gram :initarg :gram
         :initform nil
         :reader gram)
   (morphology :initarg :morphology
               :initform nil
               :reader morphology)
   (aerobicity :initarg :aerobicity
               :initform nil
               :reader aerobicity)
   (growth-conformation :initarg :growth-conformation
                        :initform nil
                        :reader growth-conformation)))


