; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-printers.l
; Description:  printing functions for grammar debugging
; Author:       Joachim H. Laubsch
; Created:       4-Aug-92
; Modified:     Wed Sep  7 17:40:30 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/printers.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: printers.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(IN-PACKAGE  "ZEBU")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     printing the internals of a grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-actions (grammar &optional (stream t))
  (let ((g (find-grammar (string grammar))))
    (if (null g)
	(error "No Grammar named ~S loaded" grammar)
      (let ((*package* (find-package (grammar-package g)))
	    (zb-rules (grammar-zb-rules g)))
	(dotimes (i (length zb-rules))
	  (let ((pair (svref zb-rules i)))
	    (format stream "~%~%Rule: ~S" (car pair))
	    (dolist (prod (zb-rule--productions (cdr pair)))
	      (let ((action (production-rhs--build-fn prod)))
                #+MCL (print action stream)
		#-MCL (pprint action stream)))))
	(values)))))

(defun print-production (prod)
  (format t "~A: ~A -> "
	  (production-index prod) (g-symbol-name (lhs prod)))
  (dolist (x (rhs prod))
    (princ (g-symbol-name x)) (princ " ")))

(defun print-productions ()
  (dolist (x (reverse *productions*))
    (print-production x) (terpri)))

(defun print-symbols ()
  (dolist (sym (reverse *symbols*))
    (format t "~A: ~A~%" (g-symbol-index sym) (g-symbol-name sym)))
  )

(defun print-own-productions (sym)
  (dolist (x (g-symbol-own-productions sym))
    (print-production x) (terpri)))

(defun print-rhs-productions (sym)
  (dolist (x (g-symbol-rhs-productions sym))
    (print-production x) (terpri)))

(defun cruise-symbols ()
  (dolist (sym (reverse *symbols*))
    (format t "~%~A: ~A~%"
	    (g-symbol-index sym)
	    (g-symbol-name sym))
    (when (g-symbol-own-productions sym)
      (format t "Own productions:~%")
      (print-own-productions sym))
    (when (g-symbol-rhs-productions sym)
      (format t "RHS productions:~%") 
      (print-rhs-productions sym))
    (princ "----------------------------")
    ))

(defun cruise-symbols-2 ()
  (terpri)
  (dotimes (i (length *symbol-array*))
    (let ((sym (svref *symbol-array* i)))
      (format t "~S: ~S~%"
	      (g-symbol-index sym)
	      (g-symbol-name sym)))))

(defun cruise-follow-sets ()
  (let (*print-circle*)
    (dolist (gs *symbols*)
      (when (g-symbol-non-terminal? gs)
	(format t "~%~A: ~S~%--------------------"
		gs
		(oset-item-list (g-symbol-follow-set gs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-collection (closures-too?)
  (format t "~%Start state index: ~A~%" *lr0-start-state-index*)
  (oset-for-each
   #'(lambda (item-set)
       (format t "------------------ ~A -------------------~%"
	       (item-set-index item-set))
       (item-set-print-kernel item-set closures-too?)
       (let ((gotos (item-set-goto-map item-set)))
	 (when (oset-item-list gotos)
	   (princ "gotos: ")
	   (oset-for-each
	    #'(lambda (gmelt)
		(format t "~A -> ~A  "
			(g-symbol-name (car gmelt))
			(item-set-index (cdr gmelt))))
	    gotos)
	   (terpri)))
       )
   *lr0-item-sets*))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun item-print (item &optional (stream t) level)
  ;; This only prints the lr(0) parts and the lookaheads.
  (declare (ignore level))
  (let ((after-dot (item-after-dot item))
	(production (item-production item)))
    (format stream "~A -> " (g-symbol-name (lhs production)))
    (do ((ncdr (rhs production) (cdr ncdr))
	 (i 0 (1+ i)))
	((null ncdr)
	 (when (= after-dot i) (princ ". "))
	 (unless (oset-empty? (item-look-aheads item))
	   (princ "{ "  stream)
	   (oset-for-each
	    #'(lambda (gs) (format stream "~A " (g-symbol-name gs)))
	    (item-look-aheads item))
	   (princ "}"  stream)))
      (format stream "~:[~;. ~]~A "
	      (= after-dot i)
	      (g-symbol-name (car ncdr))))))

(defun item-list-print (item-list)
  (dolist (item item-list)
    (terpri)
    (item-print item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cruise-parse-tables ()
  (format t "Start-state is ~S" *lr0-start-state-index*)
  (dotimes (i *lr0-item-set-count*)
    (format t "~%~A~%actions: " i)
    (oset-for-each
     #'(lambda (action-elt)
	 (format t "~A : ~A ~A  "
		 (get-print-name (car action-elt))
		 (cadr action-elt)
		 (caddr action-elt)))
     (svref (the vector *action-array*) i))
    (format t "~%gotos: ")
    (oset-for-each
     #'(lambda (goto-elt)
	 (format t "~A : ~A  "
		 (get-print-name (car goto-elt))
		 (cdr goto-elt))
	 )
     (svref (the vector *goto-array*) i))
    (format t "~%--------------------------------------------------")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test:
#||
 (load "zebu-loadgram")
 (load-grammar "ex1.grm")
 (print-symbols)
 (cruise-symbols)
 (cruise-symbols-2)
 (print-productions)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           End of zebu-printers.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
