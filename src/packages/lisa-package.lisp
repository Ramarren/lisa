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

;;; File: lisa-package.lisp
;;; Description: Package support code for LISA.

;;; $Id: lisa-package.lisp,v 1.3 2001/09/06 15:51:35 youngde Exp $

(in-package "LISA")

(defparameter *lisa-exports*
    `("DEFRULE" "DEFTEMPLATE" "ASSERT" "DEFIMPORT" "FACTS" "RULES" "AGENDA"
    "RESET" "CLEAR" "RUN" "RETRACT" "MODIFY" "WATCH" "UNWATCH" "WATCHING"
    "HALT" "ASSERT-INSTANCE" "RETRACT-INSTANCE" "MARK-INSTANCE-AS-CHANGED"
    "TELL-LISA-MODIFIED-INSTANCE" "SLOT" "TEST" "ENGINE" "USE-ENGINE"
    "USE-DEFAULT-ENGINE" "CURRENT-ENGINE" "WITH-INFERENCE-ENGINE"
    "MAKE-INFERENCE-ENGINE" "ASSERT-FROM-STRING" "USE-LISA" "=>"
    "*SHOW-LISA-WARNINGS*"))

(defparameter *lisa-shadow-imports*
    '("ASSERT"))

(defun use-lisa (package)
  (flet ((make-symbol-list (symbol-names)
           (mapcar #'(lambda (name)
                       (find-symbol name "LISA"))
                   symbol-names)))
    (let ((pkg (if (packagep package)
                   package
                 (find-package package))))
      (shadowing-import (make-symbol-list *lisa-shadow-imports*) pkg)
      (import (make-symbol-list *lisa-exports*) pkg))))
