; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebra-debug.lisp
; Description:  Translating KB-Objects into readable lists 
; Author:       Karsten Konrad
; Created:       6-Apr-93
; Modified:     Wed Aug  3 12:48:51 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/zdebug.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1993, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zdebug.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      PostScript Graph of the Kb-domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; psgraph (from CMU) and Ghostview from FSF are needed
#+LUCID
(defun show-kb-hierarchy (&optional (file "/tmp/kb-classes.ps"))
  (let ((start 'kb-domain))
    (with-open-file (*standard-output* file :direction :output)
      (psgraph start 
	       #'zb:KB-subtypes
	       #'(lambda (x) (list (string x)))
	       t nil #'eq))
    (shell (format
	    nil
	    "ghostview -display ~a -notitle -nolabels -nolocator ~a &"
	    (environment-variable "DISPLAY") file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Als Zugabe eine Funktion, die ein Kb-Objekt in eine vollstaendige
; Liste uebersetzt; man sieht dann mal, was alles in der Struktur
; steht. Vor allem zum Debuggen von Transformationen ist das
; sehr hilfreich.

(require "zebu-kb-domain")
(require "zebu-tree-attributes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translating KB-Objects into readable lists 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kb-tree2list (kb-object)
  "translates a kb-object to a list which should contain
   all relevant information."
  (cond ((kb-domain-p kb-object)
         (cons (type-of kb-object) (kb-kids2list kb-object)))
        ((consp kb-object)
         (mapcar #'kb-tree2list kb-object))
        (t kb-object)))

(defun kb-kids2list (kb-object)
  "conses reader-fn and childs into a description list"
  (let ((childs nil)
        (ta (KB-tree-attributes (type-of kb-object))))
    (when ta
      (dolist (reader (the list (first ta)))
        (push (list reader
                    (kb-tree2list (funcall reader kb-object))) childs))
    (nreverse childs))))

(defun print-readform (kb-object)
  "prints a kb-object in a readable form"
  (pprint (kb-tree2list kb-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      End of zebra-debug.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
