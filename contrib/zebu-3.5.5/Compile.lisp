; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         Compile.lisp
; Description:  Compile a CL file with Zebu runtime system loaded
; Author:       Joachim H. Laubsch
; Created:      30-Oct-92
; Modified:     Wed Feb 17 15:55:22 1993 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/Compile.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: Compile.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "CL-USER")

(declaim (optimize (speed 3) (safety 1) (compilation-speed 0)))

(let ((*default-pathname-defaults*
       (make-pathname :directory (pathname-directory *LOAD-PATHNAME*)
		      :type "lisp"))
      ifile ofile)
  (do* ((i 2 (1+ i))
	(arg (command-line-argument i) (command-line-argument i)))
       ((null arg)
	(unless ifile (error "No -f argument found")))
    (cond ((equal arg "-f")
	   (setq ifile (command-line-argument (incf i))))
	  ((equal arg "-o")
	   (setq ofile (command-line-argument (incf i))))))
  (if ofile
      (compile-file ifile :output-file ofile)
    (compile-file ifile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             End of Compile.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



