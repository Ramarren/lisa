; -*- mode:     CL -*- ------------------------------------------------- ;
; File:         empty-st.l
; Description:  Conversion to CL of the original Scheme program by (W M Wells)
; Author:       Joachim H. Laubsch
; Created:      31-Oct-90
; Modified:     Tue Jan 26 09:20:23 1993 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-empty-st.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-empty-st.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cruise the productions and figure out which ones derive the empty string.

(defun calculate-empty-string-derivers ()
  (labels ((string-vanishes (gslist)
	     (cond ((null gslist) t)
		   ((not (g-symbol-derives-empty-string (car gslist))) nil)
		   (T (string-vanishes (cdr gslist)))))
	   (process-symbol-which-derives-empty-string (gs)
	     (unless (g-symbol-derives-empty-string gs)
	       (let (*print-circle*)
		 (format t "~S derives the empty string~%" gs))
	       (setf (g-symbol-derives-empty-string gs) t)
	       (dolist (prod (g-symbol-rhs-productions gs))
		 (if (string-vanishes (rhs prod))
		     (process-symbol-which-derives-empty-string (lhs prod)))))))
    (dolist (prod *productions*)
      (unless (rhs prod)
	(process-symbol-which-derives-empty-string (lhs prod))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test:
#||
(load-grammar (merge-pathnames "ex3.zb" *ZEBU-test-directory*))
(calculate-empty-string-derivers)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              End of empty-st.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
