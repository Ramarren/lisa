; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-regex.l
; Description:  A Lisp based Regular Expression Compiler
; Author:       Joachim H. Laubsch
; Created:      21-Sep-92
; Modified:     Mon Apr 18 13:38:26 1994 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
;
; (c) Copyright 1992, Hewlett-Packard Company
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Hewlett-Packard Company
;;; makes no warranty about the software, its performance or its conformity
;;; to any specification.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: regex.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
; 13-Jan-93 (Joachim H. Laubsch)
;  Aletrnatives, to be indicated by \| need to be done!
;  7-Oct-92 (Joachim H. Laubsch)
;  made . fail on Newline in String
; 28-Sep-92 (Joachim H. Laubsch)
;  made ? work when it occured after a string (similar to the cases for +,*)
; 21-Sep-92 (Joachim H. Laubsch)
;  made behavior conform more with Emacs Lisp's STRING-MATCH
;  e.g. (string-match "\\(na\\)x\\1" "naxnana") matches now,
;  but before (string-match "(na)x\\1" "naxnana") did. 
;  "\(" is the grouping construct, and since \ is the quoting character,
;  it must be qoted as well, giving "\\(".
;  Avoided string-copying by introducing pointers in the match group case.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode:Common-Lisp; Package:ZEBU; Base:10 -*-
;;;
;;; This code was written by:
;;;
;;;    Lawrence E. Freil <lef@nscf.org>
;;;    National Science Center Foundation
;;;    Augusta, Georgia 30909
;;;
;;; If you modify this code, please comment your modifications
;;; clearly and inform the author of any improvements so they
;;; can be incorporated in future releases.
;;;
;;; nregex.lisp - My 4/8/92 attempt at a Lisp based regular expression
;;;               parser. 
;;;
;;;               This regular expression parser operates by taking a
;;;               regular expression and breaking it down into a list
;;;               consisting of lisp expressions and flags.  The list
;;;               of lisp expressions is then turned into a
;;;               lambda expression that can be later applied to a
;;;               string argument for parsing.


(in-package "ZEBU")
(provide "zebu-regex")

;;;
;;; Declare the global variables for storing the paren index list.
;;;
(declaim (special *regex-groups* *regex-groupings*))

;; In Gnu Emacs Lisp's regular expressions the braces: {,} are not special,
;; neither are the parens: (,), nor the alternatives char: |
;;(defvar *regex-special-chars* "?*+.()[]\\${}")
(defvar *regex-special-chars* "?*+.[]\\$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                For debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Declare some simple macros to make the code more readable.
;;;
(defvar *regex-debug* nil)		; Set to nil for no debugging code

(defmacro info (message &rest args)
  (if *regex-debug*
      `(format *standard-output* ,message ,@args)))

(eval-when (compile)
  (setq *regex-debug* nil))

;;;
;;; Declare a simple interface for testing.  You probably wouldn't want
;;; to use this interface unless you were just calling this once.
;;;
#||
(defun regex (expression string)
  "Usage: (regex <expression> <string)
   This function will call regex-compile on the expression and then apply
   the string to the returned lambda list."
  (let ((findit (cond ((stringp expression)
		       (regex-compile expression))
		      ((listp expression)
		       expression)))
	(result nil))
    (if (not (funcall (if (functionp findit)
			  findit
			(eval `(function ,findit))) string))
	(return-from regex nil))
    (if (= *regex-groupings* 0)
	(return-from regex t))
    (dotimes (i *regex-groupings*)
      (push (funcall 'subseq 
		     string 
		     (car (svref *regex-groups* i))
		     (cadr (svref *regex-groups* i)))
	    result))
    (reverse result)))

||#

;; specialized to the :anchored T case
;; returns just the body of the fn with STRING, START, END free.

(defun regex-compile (source &aux (ln-source (length source)))
  "Usage: (regex-compile <regular expression>)"
  ;; If the expression was an empty string then it always
  ;; matches (so lets leave early)
  (when (= ln-source 0) (return-from regex-compile '(t)))
  (macrolet ((add-exp (list)
	       ;; Add an item to the end of expression
	       `(setf expression-ln (+ expression-ln (length ,list))
                      expression (append expression ,list)))
	     (add-exp1 (item)
	       `(setf expression-ln (1+ expression-ln)
                      expression (nconc expression (list ,item)))))
    
    (info "Now entering regex-compile with ~S~%" source)
    ;;
    ;; This routine works in two parts.
    ;; The first pass take the regular expression and produces a list of 
    ;; operators and lisp expressions for the entire regular expression.  
    ;; The second pass takes this list and produces the lambda expression.
    (let ((expression			; holder for expressions
	   ;;
	   ;; Generate the very first expression to save the starting index
	   ;; so that group 0 will be the entire string matched always
	   ;;
	   (list '(setf (svref *regex-groups* 0) (list index nil))))
          (expression-ln 1)		; length of expression
	  (group 1)			; Current group index
	  (group-stack nil)		; Stack of current group endings
	  (result nil)			; holder for built expression.
	  )

      ;; If the first character is a literal, then do a quick scan to see
      ;; if it is even in the string.
      ;; If not then we can issue a quick nil, 
      ;; otherwise we can start the search at the matching character to skip
      ;; the checks of the non-matching characters anyway.
      ;;
      ;; If I really wanted to speed up this section of code it would be 
      ;; easy to recognize the case of a fairly long multi-character literal
      ;; and generate a Boyer-Moore search for the entire literal. 
      ;;
      ;; I generate the code to do a loop because on CMU Lisp this is about
      ;; twice as fast a calling position.
      ;;

      ;;
      ;; Loop over each character in the regular expression building the
      ;; expression list as we go.
      ;;
      (do ((eindex 0 (1+ eindex)))
	  ((= eindex ln-source))
	(let ((current (char source eindex)))
	  (info "Now processing character ~A index = ~A~%" current eindex)
	  (case current
	    (#\.
	     ;;
	     ;; Generate code for a single wild character
	     ;;
	     (add-exp1 '(if (>= index length)
			 (return-from compare nil)
			 (incf index)))
	     )
	    (#\$
	     ;;
	     ;; If this is the last character of the expression then
	     ;; anchor the end of the expression, otherwise let it slide
	     ;; as a standard character (even though it should be quoted).
	     ;;
	     (if (= eindex (1- ln-source))
		 (add-exp1 '(if (/= index length)
			     (return-from compare nil)))
	       (add-exp1 '(if (and (< index length)
			       (eql (char string index) #\$))
			   (incf index)
			   (return-from compare nil)))))
	    (#\* (add-exp1 'ASTERIX))

	    (#\+ (add-exp1 'PLUS))

	    (#\? (add-exp1 'QUESTION))

	    (#\[
	     ;;
	     ;; Start of a range operation.
	     ;; Generate a bit-vector that has one bit per possible character
	     ;; and then on each character or range, set the possible bits.
	     ;;
	     ;; If the first character is carat then invert the set.
	     (let* ((invert (eql (char source (1+ eindex)) #\^))
		    (bitstring (make-array
				256
				:element-type 'bit
				:initial-element (if invert 1 0)))
		    (set-char (if invert 0 1)))
	       (if invert (incf eindex))
	       (let (hi-char)
		 (do* ((x (1+ eindex) (1+ x))
		       (char (char source x)
			     (if (= x ln-source)
				 (error "No closing \"]\" found in ~a"
					source)
			       (char source x))))
		      ((eql char #\]) (setf eindex x))
		   (info "Building range with character ~A~%" (char source x))
		   (if (let ((x+2 (+ x 2)))
			 (and (< x+2 ln-source)
			      (eql (char source (1+ x)) #\-)
			      (not (char= (setf hi-char (char source x+2))
					  #\]))))
		       (progn
			 (if (char>= char hi-char)
			     (error "Invalid range \"~A-~A\".  Ranges must be in acending order"
				    char hi-char))
			 (do ((j (char-code char) (1+ j)))
			     ((> j (char-code hi-char))
			      (incf x 2))
			   (info "Setting bit for char ~A code ~A~%" (code-char j) j)
			   (setf (sbit bitstring j) set-char)))
		     (progn
		       ;;
		       ;; If the character is quoted then find out what
		       ;; it should have been
		       ;;
		       (when (char= char #\\)
			 (let (length)
			   (multiple-value-setq (char length)
			     (regex-quoted (subseq source (1+ x)) invert))
			   (incf x length)))
		       (info "Setting bit for char ~C code ~A~%"
			     char (char-code char))
		       (if (vectorp char)
			   (bit-ior bitstring char t)
			 (setf (sbit bitstring (char-code char))
			       set-char))))))
	       (add-exp1 `(let ((range ,bitstring))
			   (if (>= index length)
			       (return-from compare nil))
			   (if (= 1 (sbit range (char-code (char string index))))
			       (incf index)
			     (return-from compare nil))))))
	    (#\\
	     ;;
	     ;; Intrepret the next character as a special, range, octal, group or 
	     ;; just the character itself.
	     ;;
	     (multiple-value-bind (value length)
		 (regex-quoted (subseq source (1+ eindex)) nil)
	       (cond ((listp value) (add-exp value))
		     ((characterp value)
		      (case value
			(#\(
			 ;;
			 ;; Start a grouping.
			 ;;
			 (incf group)
			 (push group group-stack)
			 (add-exp1 `(setf (svref *regex-groups* ,(1- group)) 
				     (list index nil)))
			 (add-exp1 group))
			(#\)
			 ;;
			 ;; End a grouping
			 ;;
			 (let ((group (pop group-stack)))
			   (add-exp1 `(setf (cadr (svref *regex-groups* ,(1- group)))
				       index))
			   (add-exp1 (- group))))
			(t (add-exp1 `(if (and (< index length)
					   (eql (char string index) 
					    ,value))
				       (incf index)
				       (return-from compare nil))))))
		     ((vectorp value)
		      (add-exp1 `(let ((range ,value))
				  (if (>= index length)
				      (return-from compare nil))
				  (if (= 1 (sbit range (char-code (char string index))))
				      (incf index)
				    (return-from compare nil))))))
	       (incf eindex length)))
	    (t
	     ;;
	     ;; We have a literal character.  
	     ;; Scan to see how many we have and if it is more than one
	     ;; generate a string= verses as single eql.
	     ;;
	     (let* ((lit "")
		    (term (dotimes (litindex (- ln-source eindex) nil)
			    (let ((litchar (char source (+ eindex litindex))))
			      (if (position litchar *regex-special-chars*)
				  (return litchar)
				(progn
				  (info "Now adding ~A relative index ~A to lit~%"
					litchar litindex)
				  (setf lit (concatenate 'string lit 
							 (string litchar)))))))))
	       ;;(break "lit: ~S term: ~S" lit  term)
	       (if (= (length lit) 1)
		   (progn
		     (add-exp1 `(if (and (< index length)
				     (eql (char string index)
				      ,current))
				 (incf index)
				 (return-from compare nil))))
		 ;;
		 ;; If we have a multi-character literal then we must
		 ;; check to see if the next character (if there is one)
		 ;; is an asterix or a plus.  If so then we must not use this
		 ;; character in the big literal.
		 (progn
		   (when (member term '(#\* #\+ #\?))
		     (setf lit (subseq lit 0 (1- (length lit)))))
		   (if (= (length lit) 1)
		       (add-exp1 `(if (and (< index length)
				       (eql (char string index)
					,(schar lit 0)))
				   (incf index)
				   (return-from compare nil)))
		     (progn
		       (add-exp1 `(let ((new-index (+ index ,(length lit))))
				   (if (< length new-index)
				       (return-from compare nil))
				   (if (string= string ,lit :start1 index
						:end1 new-index)
				       (incf index ,(length lit))
				     (return-from compare nil))))
		       (incf eindex (1- (length lit))))))))))))
      ;;
      ;; Plug end of list to return t.  If we made it this far then
      ;; We have matched!
      (add-exp1 '(setf (cadr (svref *regex-groups* 0)) index))
      (add-exp1 '(return-from final-return t))
      ;;
      ;;
      ;; Now take the expression list and turn it into a lambda expression
      ;; replacing the special flags with lisp code.
      ;; For example:  A BEGIN needs to be replaced by an expression that
      ;; saves the current index, then evaluates everything till it gets to
      ;; the END then save the new index if it didn't fail.
      ;; On an ASTERIX I need to take the previous expression and wrap
      ;; it in a do that will evaluate the expression till an error
      ;; occurs and then another do that encompases the remainder of the
      ;; regular expression and iterates decrementing the index by one
      ;; of the matched expression sizes and then returns nil.  After
      ;; the last expression insert a form that returns t so that
      ;; if the entire nested sub-expression succeeds then the loop
      ;; is broken manually.
      ;; 
      ;;
      ;; Reversing the current expression makes building up the 
      ;; lambda list easier due to the nesting of expressions when 
      ;; an asterisk has been encountered.
      (setf expression (reverse expression))
      (info "~&Regular Expression:~%(~{~s~% ~}) ;; ~d"
	    expression expression-ln)

      (do ((elt 0 (1+ elt))) 
          ((= elt expression-ln))
	(let ((piece (nth elt expression))
	      (piece+1 (nth (1+ elt) expression)))
	  ;;
	  ;; Now check for PLUS, if so then ditto the expression and then let the
	  ;; ASTERIX below handle the rest.
	  ;;
	  ;; (princ ".")
	  (when (eql piece 'PLUS)
	    (cond ((listp piece+1) (push piece+1 result))
		  ;;
		  ;; duplicate the entire group
		  ;; NOTE: This hasn't been implemented yet!!
		  (t (warn "~%GROUP repeat hasn't been implemented yet~%"))))
	  (cond ((listp piece)		; Just append the list
		 (push piece result))
		((eql piece 'QUESTION)	; Wrap it in a block that won't fail
		 (cond ((listp piece+1)
			(push `(progn (block compare ,piece+1)
				t)
			      result)
			(incf elt))
		       ;;
		       ;; This is a QUESTION on an entire group which
		       ;; hasn't been implemented yet!!!
		       ;;
		       (t
			(warn "~%Optional groups not implemented yet~%"))))
		((or (eql piece 'ASTERIX) ; Do the wild thing!
		     (eql piece 'PLUS))
		 (when (listp piece+1)
		   ;;
		   ;; This is a single character wild card so
		   ;; do the simple form.
		   ;;
		   (setf result 
			 `((let ((oindex index))
			     (block compare
			       (do nil (nil) ,piece+1))
			     (do ((start index (1- start)))
				 ((< start oindex) nil)
			       (let ((index start))
				 (block compare
				   ,@result))))))
		   (incf elt))))))	; Just ignore everything else.

      (info "~&Result:~s" result)
      ;;
      ;; Now wrap the result in a lambda list that can then be 
      ;; invoked or compiled, however the user wishes.
      ;;
      (setf result
	    `((setf *regex-groupings* ,group)
	      (block final-return
		(block compare
		  (let ((index start)
			(length end))
		    ,@result))))))))


;;;
;;; Define a function that will take a quoted character and return
;;; what the real character should be plus how much of the source
;;; string was used.  If the result is a set of characters, return an
;;; array of bits indicating which characters should be set.  If the
;;; expression is one of the sub-group matches, return a
;;; list-expression that will provide the match.  
;;;

(defun regex-quoted (char-string &optional (invert nil))
  "Usage: (regex-quoted <char-string> &optional invert)
       Returns either the quoted character or a simple bit vector of bits set for
       the matching values"
  (let ((first (char char-string 0))
	(used-length 1)
	result)
    (setf result
	  (case first
	    (#\n #\NewLine)
	    (#\c #\Return)
	    (#\t #\Tab)
	    (#\d #*0000000000000000000000000000000000000000000000001111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
	    (#\D #*1111111111111111111111111111111111111111111111110000000000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)
	    (#\w #*0000000000000000000000000000000000000000000000001111111111000000011111111111111111111111111000010111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
	    (#\W #*1111111111111111111111111111111111111111111111110000000000111111100000000000000000000000000111101000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)
	    (#\b #*0000000001000000000000000000000011000000000010100000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
	    (#\B #*1111111110111111111111111111111100111111111101011111111111011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)
	    (#\s #*0000000001100000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
	    (#\S #*1111111110011111111111111111111101111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)
	    (t (if (and (char>= first #\0) (char<= first #\9))
		   (if (and (> (length char-string) 2)
			    (and (char>= (char char-string 1) #\0)
				 (char<= (char char-string 1) #\9)
				 (char>= (char char-string 2) #\0)
				 (char<= (char char-string 2) #\9)))
		       ;;
		       ;; It is a single character specified in octal
		       ;;
		       (parse-integer char-string
				      :end (setf used-length 3)
				      :radix 8 :junk-allowed t)
	     
		     ;;
		     ;; We have a group number replacement.
		     ;;
		     (let ((group (- (char-code first) (char-code #\0))))
		       `((let* ((range (svref *regex-groups* ,group))
				(start-old (car (the cons range)))
				(end-old (cadr (the cons range)))
				(ln-nstring (- end-old start-old))
				(new-index (+ index ln-nstring)))
			   (if (< length new-index)
			       (return-from compare nil))
			   (if (string= string string
					:start1 start-old
					:end1   end-old
					:start2 index
					:end2   new-index)
			       (setq index new-index)
			     (return-from compare nil)))))) 
		 first))))
    (if (and (vectorp result) invert)
	(bit-xor result #*1111111110011111111111111111111101111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 t))
    (values result used-length)))

#||
(defun match-beginning (n)
  (first (SVREF *REGEX-GROUPS* n)))

(defun match-end (n)
  (second (SVREF *REGEX-GROUPS* n)))
||#

(defun def-regex-parser (name pattern)
  (when (and (eql (symbol-package name) (find-package "LISP"))
	     (fboundp name))
    (error "A lexical category should not name a Lisp function: ~s"
	   name))
  (let* ((body (regex-compile pattern)))
    `(defun ,name (STRING &optional (START 0) (END (length STRING)))
      ,@(when *regex-debug*
	  '((info "~%Looking at: ~S..."
	     (subseq string START (min (+ 10 START) END)))))
      (when (progn .,body)
	(second (SVREF *REGEX-GROUPS* 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             End of zebu-regex.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
