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

;;; File: deftemplate.lisp
;;; Description: This class serves as the base for all internally-generated
;;; "template" classes. LISA generates one of these classes whenever a fact is
;;; supported by a DEFTEMPLATE form rather than an actual CLOS class.

;;; $Id: deftemplate.lisp,v 1.1 2001/03/13 18:44:29 youngde Exp $

(in-package :lisa)

(defclass deftemplate () ())

