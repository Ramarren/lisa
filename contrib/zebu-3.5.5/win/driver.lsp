; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-driver.lisp
; Description:  Conversion to CL of the original Scheme program (by W. M Wells)
; Author:       Joachim H. Laubsch
; Created:      10-Oct-90
; Modified:     Thu Oct  2 09:58:20 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/Attic/driver.lsp,v 1.1 2000/10/17 18:03:33 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: driver.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:33  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
; 26-Jul-95 (Joachim H. Laubsch)
;  a user defined category, that can also be read as a keyword is
;  only identified if it is longer than a keyword
; 25-Apr-94 (Joachim H. Laubsch)
;  implemented state-sensitive token look-ahead
; 17-Aug-93 (Joachim H. Laubsch)
;  read-parser recognizes numbers: integer, ratio and float
;    <digit>* [ "." <digit>+ ]
;    <digit>+ "/" <digit>+
; the boolean id-allows-start-digit determines for a grammar whether an
; identifier may start with a digit.
; 22-Feb-93 (Joachim H. Laubsch)
;  if the grammar's intern-identifier attribute is true (default), an
;  Identifier will be represented as a symbol, otherwise a string
;  2-Feb-93 (Joachim H. Laubsch)
;  introduce the variable *case-sensitive* to deal with grammars whith
;  case-sensitive keywords
; 13-Jan-93 (Joachim H. Laubsch)
;  rewrote recognize-token so that (in ALL cases) keys that could start an
;  identifier will not be recognized as keys, but as identifiers.
; 27-Nov-92 (Joachim H. Laubsch)
;  Added Variable *preserve-case*
;  "If true, the case of an identifier will be preserved (default false)."
; 29-Sep-92 (Joachim H. Laubsch)
;  a one-character keyword is considered a token iff it is not
;  in identifier-start-chars or if the next character is not in
;  identifier-continue-chars
; 21-Jul-92 (Joachim H. Laubsch)
;  improved handling of NUMBER and IDENTIFIER in next-token
; 27-Apr-92 (Joachim H. Laubsch)
;  introduce *COMMENT-START*, a character that causes everything following
;      until the end-of-line to be ignored
;  introduce *COMMENT-BRACKETS*, a list of pairs of strings that designate
;      everything between them as to be ignored
; 22-Apr-92 (Joachim H. Laubsch)
;  define FILE-PARSER, a function like READ-PARSER that takes input
;  from a file instead of from a string
;  introduced :junk-allowed as argument to READ-PARSER with same meaning
;  as that keyword in READ-FROM-STRING
;  analogously in LIST-PARSER
; 15-Apr-92 (Joachim H. Laubsch)
;  introduce *IDENTIFIER-START-CHARS*
; 30-Oct-91 (Joachim H. Laubsch)
;  improved error checking in case a grammar does not use NUMBER, but the
;  parser will be given strings containing NUMBERs
; 16-Jul-91 (Joachim H. Laubsch)
;  Added a facility to deal with multiple grammars
;  lr-parse takes a third argument, a grammar
;  READ-PARSER and LIST-PARSER take a :grammar keyword argument, defaulting to
;  *current-grammar*
; 26-Jun-91 (Joachim H. Laubsch)
;  Added a proposal to distinguish String and Symbol-tokens in lexical analysis
;  of read-parser.  See comments with section
;         *string-delimiter*, *symbol-delimiter*
; 25-Apr-91 (Joachim H. Laubsch)
;  fixed bug in read-parser which caused scanner to break if a number was the
;  last constituent of a string
; 26-Mar-91 (Joachim H. Laubsch)
;  in the case where a keyword is found, but no action defined, we
;  assume it must be an identifier.  If there is an action entry for
;  an identifier, that identifier is interned from the keyword string read
; 26-Mar-91 (Joachim H. Laubsch)
;  make read-parser read these types of numbers: integer, float, rational
;  1-Mar-91 (Joachim H. Laubsch)
;  made various simple changes, based on monitoring results to speed up
;  READ-PARSER by a factor of 10
; 30-Jan-91 (Joachim H. Laubsch)
;  introduce variable: *string-delimiter*
; 17-Jan-91 (Joachim H. Laubsch)
;  introduced String syntax:  "Fred Jones" is a nll-constant
; 11-Dec-90 (Joachim H. Laubsch)
;  introduced the ZEBU package, and imported its exported symbols into USER
;  7-Dec-90 (Joachim H. Laubsch)
;  if a keyword ending in a symbol-continue-char is followed by a 
;  symbol-continue-char a keyword token is NOT recognized (but an identifier)
;  except if there would have been a single character keyword recognizing the 
;  same initial substring. E.g. ?u?foo1 is tokenized as ?u?, foo1, because
;  there is the shorter keyword alternative: ?, u?foo1
;  The principle is to give priority to the longest possible keyword.
;  (Note that agt007 or agt?x are recognized as identifiers)
; 27-Nov-90 (Joachim H. Laubsch)
;  Lexical Analysis (recognize-token) will recognize any keyword of the
;  language.  If lr-parse is given a token that is a keyword, it may not have
;  an action for it, but if this same token were regarded as an identifier,
;  there would be one.  Instead of reporting an error, lr-parse will now look 
;  first for the identifier-action.  
;    It would be best, if lr-parse could predict, whether an identifier is legal
;  in the current state and then direct recognize-token appropriately.  I should
;  come back to this, and implement that.  It would also save time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Written by William M. Wells.  This is an example lr parser driver
;;; which uses parse table files generated by Zebu.  

(in-package "ZEBU")

(provide "zebu-driver")
(require "zebu-loader")
;;;
;;; A rudimentary lr parser driver.
;;; It has provisions for applying client supplied procedures which are
;;; associated with productions in the grammar.
;;;
;;;
;;; This code is independent of the parse table generating system,
;;; and basically stand alone,  although
;;; it needs some macros defined in other files.
;;;
(defvar *CURRENT-GRAMMAR* *NULL-Grammar*)

(defvar *terminal-alist-SEQ*)

(defvar *lexer-debug* nil)
(eval-when (compile)
  (setq *lexer-debug* nil))

#|
(setq *lexer-debug* t)
|#

(defmacro if-debugging-lexer (then &optional else)
  `,(if *lexer-debug* then else))

(if-debugging
 (defmacro say-looking-at ()
   '(format t "~%Looking-at: ~S . ~a {~s}"
     input-symbol-instantiation
     (let ((a (svref (grammar-lexicon grammar) input-symbol-index)))
       (if (symbolp a) (format nil "<~a>" (symbol-name a)) a))
     input-symbol-index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (upcased-subseq string from to) == (string-upcase (subseq string from to))
;; but avoids a copy
(defun upcased-subseq (string beg end)
  (declare (simple-string string) (fixnum beg end))
  (let* ((size (- end beg))
	 (R (make-sequence 'simple-string size))
	 (stringi beg))
    (declare (simple-string R) (fixnum stringi))
    (dotimes (index size)
      (setf (schar R index) (char-upcase (the character (schar string stringi))))
      (incf stringi))
    R))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             The LR parser itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; symbol-stack and state-stack are the standard things for an lr parser.
;;; the client lambdas and stack are used in the following fashion:
;;;
;;;   When a shift action occurs, the instantiation of the input symbol
;;;   is pushed onto the client stack.
;;;
;;;   When a reduce action occurs, as many items as are on the lhs
;;;   of the associated production are popped from the client stack
;;;   and the corresponding client lambda is applied to the popped
;;;   items.  The result of the application is then pushed onto the 
;;;   client stack.  One may of course do whatever one wishes by side
;;;   effect.

;;; when junk-allowed, 2 values are returned:
;;;         the object found so far
;;;         the value returned by last-pos-fn
;;; last-pos-fn should be defined as a function that returns the place
;;;         before the token just returned by next-token-fn

;;; when more-allowed, no "<end of string>" error is issued but
;;; more-fn is called to extend the token-stream that next-token-fn is
;;; using.

(defun lr-parse (next-token-fn error-fn grammar
			       &optional junk-allowed last-pos-fn
			       &aux symbol-stack client-stack state-stack
			       action-table-top state-stack-top)
  (declare #+(or :MCL :ANSI-COMMON-LISP)
	   (dynamic-extent symbol-stack client-stack state-stack)
	   (type (or cons null) symbol-stack client-stack state-stack)
	   (type grammar grammar)
	   (type (function (simple-vector) (values t fixnum)) next-token-fn)
	   (type (function (string) error) error-fn))
  (let ((start-state (grammar-lr-parser-start-state-index grammar))
	(production-info (grammar-production-info grammar))
	(action-table (grammar-action-table grammar))
	(goto-table (grammar-goto-table grammar))
	(client-lambdas (grammar-client-lambdas grammar))
	(end-symbol-index (grammar-end-symbol-index grammar))
	action-entry)
    (declare (fixnum end-symbol-index)
	     (simple-vector action-table goto-table))
    (push start-state state-stack)
    (setf state-stack-top start-state
	  action-table-top (svref action-table start-state))
    (multiple-value-bind (input-symbol-instantiation input-symbol-index)
	(funcall next-token-fn action-table-top)
      (if-debugging (say-looking-at))
      (setf action-entry (vec-bs-assoc (the fixnum input-symbol-index)
				       action-table-top))
      (loop
       (when (null action-entry)
	 (if (eq input-symbol-index end-symbol-index)
	     (funcall error-fn
		      (undef-action-error input-symbol-instantiation
					  input-symbol-index
					  action-table-top
					  grammar))
	   (unless (and junk-allowed
			;; assume that EOF was seen
			(setq action-entry 
			      (vec-bs-assoc
			       end-symbol-index action-table-top)))
	     (or (let ((idx (grammar-identifier-index grammar)))
		   (and (setf action-entry (vec-bs-assoc idx action-table-top))
			(stringp input-symbol-instantiation)
			(not (string=
			      (the string input-symbol-instantiation) ""))
			(identifier-start-char-p
			 (schar input-symbol-instantiation 0))
			(not (find-if-not #'identifier-continue-char-p
					  input-symbol-instantiation
					  :start 1))
			(setq input-symbol-instantiation
			      (if (grammar-intern-identifier grammar)
				  (intern
				   (if *preserve-case*
				       (the string input-symbol-instantiation)
				     (string-upcase
				      (the string input-symbol-instantiation))))
				input-symbol-instantiation)
			      input-symbol-index idx)))
		 (funcall error-fn
			  (undef-action-error input-symbol-instantiation
					      input-symbol-index
					      action-table-top
					      grammar))))))	   
       ;; there should always be a non null action-entry !!
       (let ((ae-cdr (cdr (the cons action-entry))))
	 (case (car (the cons ae-cdr))
	   (:S				; Shift.
	    (setf state-stack-top (cadr ae-cdr) ; new-state
		  action-table-top (svref action-table state-stack-top))
	    (push state-stack-top state-stack)
	    (if-debugging (format t "~%Shift to ~S" state-stack-top))
	    (push input-symbol-index symbol-stack)
	    (push input-symbol-instantiation client-stack)
	    (multiple-value-setq
		(input-symbol-instantiation input-symbol-index)
	      (funcall next-token-fn action-table-top))
	    (if-debugging (say-looking-at))
	    (setf action-entry (vec-bs-assoc (the fixnum input-symbol-index)
					     action-table-top)))
	   (:R				; Reduce.
	    (let* ((prod-index (cadr ae-cdr))
		   (p (svref production-info prod-index))
		   ;; p = <lhs-symbol-index> . <production-length>
		   (prod-lhs (car (the cons p)))
		   (prod-ln (cdr (the cons p)))
		   (client-lambda (svref client-lambdas prod-index)))
	      (if-debugging (format t "~%Reduce ~S" prod-index))
	      ;; optimize simple cases
	      (case prod-ln
		(0			; Apply the client lambda and store the result.
		 (if-debugging (format t "~%; Calling ~S" client-lambda))
		 (push (funcall client-lambda) client-stack)
		 (if-debugging 
		  (let ((R (car client-stack)))
		    (format t "~%; -> ~S : ~S" R (type-of R)))))
		(1			; Apply the client lambda and store the result.
		 (when client-lambda
		   (if-debugging (format t "~%; Applying ~S to ~S"
					 client-lambda (car client-stack)))
		   (setf (car client-stack)
			 (funcall client-lambda (car client-stack)))
		   (if-debugging 
		    (let ((R (car client-stack)))
		      (format t "~%; -> ~S : ~S" R (type-of R)))))
		 (setq symbol-stack (cdr symbol-stack)
		       state-stack  (cdr state-stack)
		       ))
		(2			; Apply the client lambda and store the result.
		 (if-debugging (format t "~%; Applying ~S to ~{ ~s~}"
				       client-lambda (subseq client-stack 0 2)))
		 (when client-lambda
		   (let* ((arg2 (pop client-stack))
			  (R (funcall client-lambda
				      (car client-stack)
				      arg2)))
		     (setf (car client-stack) R)))
		 (setq symbol-stack (cddr symbol-stack)
		       state-stack  (cddr state-stack))
		 (if-debugging 
		  (let ((R (car client-stack)))
		    (format t "~%; -> ~S : ~S" R (type-of R)))))
		(t (let (constituents)
		     (dotimes (i prod-ln) 
		       (setq symbol-stack (cdr symbol-stack)
			     state-stack  (cdr state-stack))
		       (push (pop client-stack) constituents))
		     ;; Apply the client lambda and store the result.
		     (if-debugging (format t "~%; Applying ~S to ~S"
					   client-lambda constituents))
		     (push (apply client-lambda ; action
				  constituents)
			   client-stack)
		     (if-debugging 
		      (let ((R (car client-stack)))
			(format t "~%; -> ~S : ~S" R (type-of R)))))))
	      (push prod-lhs symbol-stack) ; Push lhs of production.
	      (let ((goto (cdr (the cons
				    (vec-bs-assoc
				     prod-lhs
				     (svref goto-table (car state-stack)))))))
		(if (null goto) 
		    (funcall error-fn "table error? goto not defined!"))
		(push goto state-stack)
		(setf state-stack-top goto ; new-state
		      action-table-top (svref action-table state-stack-top)
		      action-entry (vec-bs-assoc
				    (the fixnum input-symbol-index)
				    action-table-top))
		)))
	   (:A
	    ;; Accept on END symbol.
	    (if-debugging (format t "~%Accepting"))
	    ;; (break "Accept ~s" input-symbol-index)
	    (if junk-allowed
		(return
		  (values (car client-stack)
			  (when last-pos-fn (funcall last-pos-fn))))
	      (if (= input-symbol-index end-symbol-index)
		  (return
		    (values (car client-stack)
			    (when last-pos-fn (funcall last-pos-fn))))
		(if (eq input-symbol-instantiation T)
		    (funcall error-fn "Unexpected token")
		  (funcall error-fn "extra input?")))))
	   (T (funcall error-fn
		       (format nil
			       "Bogus action: ~S" (car ae-cdr))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun possible-tokens (expected lexicon)
  (if expected
      (let ((tokenL (map 'list
			 #'(lambda (action) 
			     (let ((a (svref lexicon (car action))))
			       (if (symbolp a)
				   (format nil "<~a>" (symbol-name a))
				 (format nil "~s" a))))
			 expected)))
	(format
	 nil "~%Expected~:[ one of~;~]:~{ ~a~}~%"
	 (= 1 (length tokenL)) tokenL))
    ""))

(defun unrecognized-token-error (string pos expected grammar)
  (let ((lexicon (grammar-lexicon grammar)))
    (concatenate 'string
		 (format nil "Unrecognized Token at: ~s"
			 (subseq string pos))
		 (possible-tokens expected lexicon))))

(defun undef-action-error (token index expected grammar)
  (let* ((lexicon (grammar-lexicon grammar))
	 (type (if index
		   (let ((e (svref lexicon index)))
		     (if (symbolp e)
			 (format nil "<~a>" (symbol-name e))
		       "KEY")))))
    (format
     nil "Syntax error (action not defined for token: ~S~@[ a ~a~])~a"
     token type (possible-tokens expected lexicon))))


;;; A function for looking up table entries using binary search
;;; the vector elements are the assoc key and should be in increasing order.
#||
(defun vec-bs-assoc (num vec)
  (declare (type fixnum num) (type vector vec))
  (labels ((vec-bs-assoc-aux (start end)
	     (declare (type fixnum start end))
	     (let ((start-entry (svref vec start)))
	       (declare (type cons start-entry))
	       (cond ((= num (the fixnum (car start-entry))) start-entry)
		     ((= start end) nil)
		     (T (let ((mid (floor (+ start end) 2)))
			  (declare (type fixnum mid))
			  (if (> num (the fixnum (car (svref vec mid))))
			      (vec-bs-assoc-aux (1+ mid) end)
			    (vec-bs-assoc-aux start mid))))))))
    (let ((last (1- (length (the vector vec)))))
      (declare (type fixnum last))
      (if (or (< num (the fixnum (car (svref vec 0))))
	      (> num (the fixnum (car (svref vec last)))))
	  nil
	(vec-bs-assoc-aux 0 last)))))
||#
#-ALLEGRO
(defun vec-bs-assoc (num vec)
  (declare (type fixnum num) (type simple-vector vec))
  (labels ((vec-bs-assoc-aux (start end)
	     (declare (type fixnum start end))
	     (let ((start-entry (svref vec start)))
	       (declare (type cons start-entry))
	       (cond ((= num (the fixnum (car start-entry))) start-entry)
		     ((= start end) nil)
		     (T (let ((mid (floor (+ start end) 2)))
			  (declare (type fixnum mid))
			  (if (> num (the fixnum (car (svref vec mid))))
			      (vec-bs-assoc-aux (1+ mid) end)
			    (vec-bs-assoc-aux start mid))))))))
    (let ((vln (length vec)))
      (declare (type fixnum vln))
      (if (zerop vln)
	  nil
	(let ((last (1- vln)))
	  (declare (type fixnum last))
	  (if (zerop last)
	      (let ((entry (svref vec last)))
		(declare (cons entry))
		(when (= num (the fixnum (car entry)))
		  entry))
	    (vec-bs-assoc-aux 0 last)))))))

#+ALLEGRO 
; konrad@dfki.uni-sb.de writes:
; man kann den Speicherbedarf von Zebu muehelos um mehr als 40%
; reduzieren, wenn man in zebu-driver die Definition von vec-bs-aux in
; folgendes veraendert:

(progn
  (defparameter *bs-vec* nil)
  (defparameter *bs-num* nil)

  (defun vec-bs-assoc-aux (start end)
    (declare (type fixnum start end))
    (let ((start-entry (svref *bs-vec* start)))
      (declare (type cons start-entry))
      (cond ((= *bs-num* (the fixnum (car start-entry))) start-entry)
	    ((= start end) nil)
	    (T (let ((mid (floor (+ start end) 2)))
		 (declare (type fixnum mid))
		 (if (> *bs-num* (the fixnum (car (svref *bs-vec* mid))))
		     (vec-bs-assoc-aux (1+ mid) end)
		   (vec-bs-assoc-aux start mid)))))))

  (defun vec-bs-assoc (num vec)
    (declare (type fixnum num) (type simple-vector vec))
    (setq *bs-vec* vec *bs-num* num)
    (vec-bs-assoc-aux 0 (1- (length vec))))
  )


;;; Figure out to which element of the lexicon a token corresponds.
;;; This gets a little complicated for terminal symbols which can
;;; vary at parsing time, for example, identifiers and numbers.  The way
;;; these "preterminals" are handled in this driver is as follows:
;;; If a token passes the CL test PARSE-NUMBER, and the argument number-index
;;; isn't false, then number-index is treated as representing its category.
;;; Otherwise, if the token appears exactly in the lexicon, then it is
;;; given the category of the lexicon item.  Otherwise it is assumed
;;; to be an instance of the terminal IDENTIFIER, whose presence in the
;;; lexicon is indicated by a non false value for the id-index argument.
;;; If the token isn't explicitly in the lexicon, and id-index is false,
;;; then an error is signalled.
;;; 


;;; number-index should be the index of the grammar symbol which stands
;;; for numbers, otherwise it should be false if numbers don't appear
;;; in the grammar.
;;;
;;; id-index should be the index of the grammar symbol which stands
;;; for identifiers, otherwise it should be false if identifiers don't
;;; appear in the grammar.


(defun categorize (token grammar)
  (let ((category 
	 (if (numberp token)
	     (progn (if-debugging
		     (assert (grammar-number-index grammar) ()
			     "A number was seen in the token stream"))
		    (grammar-number-index grammar))
           (let ((terminal-associations
		  (elt (grammar-terminal-alist-SEQ grammar)
		       (char-code (let ((c (schar (string token) 0)))
				    (declare (character c))
				    (if (grammar-case-sensitive grammar)
					c
				      (char-downcase c)))))))
             (if terminal-associations
		 (let ((terminal-association (assoc token terminal-associations
						    :test #'equal)))
		   (if terminal-association
		       (cdr terminal-association)
		     (grammar-identifier-index grammar)))
	       (grammar-identifier-index grammar))))))
    (values token category)))

(declaim (inline end-of-tokens-category))
(defun end-of-tokens-category (grammar)
  (values Nil (grammar-end-symbol-index grammar)))

(declaim (inline unrecognized-token-category))
(defun unrecognized-token-category (grammar)
  (values T (grammar-end-symbol-index grammar)))

;;; This implements a parser which gets its tokens from the supplied list.
;;; It uses the parsing engine lr-parse which is defined above.  It also
;;; uses the function categorize to classify tokens according to the 
;;; lexicon.

(defun list-parser (token-list &key (grammar *current-grammar*) junk-allowed)
  (let ((last-position token-list)
        token1 category)
    (flet ((list-parser-error (string)
	     (error "~a~% Remaining tokens: ~S~{ ~S~}"
		    string token1 token-list)))
      (check-type token-list list)
      (lr-parse
       ;; This lambda is the tokenizer supplied to the parsing engine:
       #'(lambda (&optional ignore)
	   (declare (ignore ignore))
	   (if (null token-list)
	       (end-of-tokens-category grammar)
	     (progn
	       (setq last-position token-list)
	       (multiple-value-setq (token1 category)
		 (categorize (pop token-list) grammar))
	       (if (null category)
		   (if junk-allowed
		       (unrecognized-token-category grammar)
		     (list-parser-error 
			(format nil "Unrecognized Token ~s" token1)))
		 (values token1 category)))))
       ;; This is the error function supplied to the parsing engine:
       #'list-parser-error
       grammar
       junk-allowed
       ;; Function that returns the remaining unparsed token-list
       #'(lambda () last-position)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 read-parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This implements a parser which gets its tokens from the Lisp function
;;; read.
;;; It uses the parsing engine lr-parse which is defined above.  It also
;;; uses the function categorize to classify tokens according to the 
;;; lexicon.  It will signal the end of input to the parser when it
;;; if it reads the end of file.

(defun read-parser (string &key
			   (error-fn #'(lambda (msg) (error "~a" msg)))
			   (print-parse-errors t)
			   (grammar *current-grammar*)
			   (start 0)
			   junk-allowed
			   more-allowed
			   more-fn)
  (declare (string string))
  (check-type string string)
  (check-type grammar grammar)
  (let ((number-index (grammar-number-index grammar))
	(identifier-index (grammar-identifier-index grammar))
	(string-index (grammar-string-index grammar))
	(string-ln (length (the string string)))
	(last-pos 0)
	(pos start)
	(end-symbol-index (grammar-end-symbol-index grammar))
	(*identifier-start-chars-V* (grammar-identifier-start-chars-V grammar))
	(id-allows-start-digit (grammar-id-allows-start-digit grammar))
	(*identifier-continue-chars-V* (grammar-identifier-continue-chars-V grammar))
	(*terminal-alist-SEQ* (grammar-terminal-alist-SEQ grammar))
	(intern-identifier (grammar-intern-identifier grammar))
	(white-space      (grammar-white-space grammar))
	(string-delimiter (grammar-string-delimiter grammar))
	(symbol-delimiter (grammar-symbol-delimiter grammar))
	(lex-cat-map (grammar-lex-cat-map grammar))
	(*case-sensitive* (grammar-case-sensitive grammar))
	token find-id? find-string?)
    (declare (fixnum string-ln pos last-pos)
	     (special *identifier-continue-chars-V*
		      *identifier-start-chars-V*))
    (flet ((white-space-p (char)
	     (member (the character char) white-space
		     :test #'char=))
	   (digit-seq? (dec end)
	     (and dec
		  (or (>= end string-ln)
		      (and (not id-allows-start-digit)
			   (not (identifier-continue-char-p
				 (schar string end)))))))
	   (new-fraction (num den places)
	     (values (float (+ num (/ den (expt 10 places))))
		     number-index))
	   )
      ;; The tokenizer supplied to the parsing engine:	   
      (flet
	  ((next-token (actionv)
	     (block next-token
	       (if-debugging
		(format t "~%~a"
			(possible-tokens actionv (grammar-lexicon grammar))))
	       (loop 
		;; skip initial blanks
		(setq last-pos pos
		      pos (or (position-if-not #'white-space-p string 
					       :start pos)
			      string-ln))
		;; end of string?
		(when (< pos string-ln) (return nil))
		(unless (and more-allowed more-fn) (return nil))
		(setq string
		      (funcall 
		       more-fn
		       #'(lambda ()
			   (if (find end-symbol-index actionv
				     :key #'car)
			       (return-from next-token
				 (values nil end-symbol-index))
			     (return-from read-parser
			       (funcall
				error-fn
				(unrecognized-token-error
				 "<end of string>" 0 actionv grammar))))))
		      string-ln (length string)
		      pos       0
		      last-pos  0))
	       (when (>= pos string-ln)
		 (if (find end-symbol-index actionv
			   :key #'car)
		     (return-from next-token
		       (values nil end-symbol-index))
		   (return-from read-parser
		     (funcall
		      error-fn
		      (unrecognized-token-error
		       "<end of string>" 0 actionv grammar)))))

	       ;; is an IDENTIFIER also expected
	       (setf find-id? (and identifier-index
				   (find identifier-index actionv
					 :key #'car)))
	       ;; scan lexical categories (regular expressions) first
	       (dolist (lex-cat-pair lex-cat-map)
		 (let ((lex-cat (car lex-cat-pair)))
		   (when (find lex-cat actionv :key #'car)
		     (let ((new-pos (funcall (the function (cdr lex-cat-pair))
					     string pos string-ln)))
		       (if-debugging-lexer
			(format t "~% calling ~s" (cdr lex-cat-pair)))
		       (when
			   (and
			    new-pos
			    ;; a match is found,
			    ;; and it could NOT be a possibly longer identifier
			    ;; and not possibly a longer keyword
			    (or 
			     (not find-id?)
			     (not
			      (and (< new-pos string-ln)
				   ;; if a identifier-continue-char doesn't
				   ;; follow, we also accept
				   (identifier-continue-char-p
				    (schar string new-pos))
				   ;; the token starts with
				   ;; an identifier-start-char
				   (identifier-start-char-p
				    (schar string pos))
				   ;; all of the remaining chars 
				   ;; continue an identifier
				   (let ((p1 (1+ pos)))
				     (declare (fixnum p1))
				     (or (= p1 new-pos)
					 (not (find-if-not
					       #'identifier-continue-char-p
					       string
					       :start p1 :end new-pos)))))))
			    ;; no possibly longer grammar keyword
			    (multiple-value-bind (token-association token-length)
				(recognize-kwd string pos string-ln actionv find-id?)
			      (if (and token-association
				       (>= new-pos (+ pos token-length)))
				  (progn
				    ;; token recognized
				    (setq pos (+ pos token-length)
					  token (car token-association))
				    (return-from next-token
				      (values token (cdr token-association)))
				    )
				t)))
			 (let ((instance (subseq string pos new-pos)))
			   (setq pos new-pos)
			   (if-debugging
			    (format t "~%LexToken: ~s : ~s ~s < ~s" instance (car lex-cat-pair) new-pos string-ln)) 
			   (return-from next-token
			     (values instance lex-cat))))))))

	       ;; read symbol, string, or number
	       ;; foo : symbol, 'foo' : symbol, "foo" : string, 3/4 : number
	       ;; recognize a number: <digit>* [ "." <digit>+ ]
	       ;;                     <digit>+ "/" <digit>+
	       (when (and number-index (find number-index actionv :key #'car))
		 (multiple-value-bind (number end)
		     (parse-integer string :start pos :junk-allowed t)
		   (if (not number)
		       ;; the case .<integer>
		       (when (and (eql (schar string pos) '#\.)
				  (DIGIT-CHAR-P (schar string (1+ pos))))
			 (multiple-value-bind (dec end)
			     (parse-integer string
					    :start (1+ pos) :junk-allowed t)
			   (when (digit-seq? dec end)
			     (let ((places (- end (1+ pos))))
			       (setq pos end)
			       (return-from next-token
				 (new-fraction 0 dec places))))))
		     (progn
		       (when (>= end string-ln)
			 (setq pos end)
			 (return-from next-token (values number number-index)))
		       (let ((c (schar string end)) (p (1+ end)))
			 (case c
			   (#\/ (multiple-value-bind (denom end)
				    (parse-integer string
						   :start p :junk-allowed t)
				  (when denom
				    (setq pos end)
				    (return-from next-token
				      (values (/ number denom) number-index))))
				(setq pos end)
				(return-from next-token
				  (values number number-index)))
			   (#\. (multiple-value-bind (dec end)
				    (parse-integer string
						   :start p :junk-allowed t)
				  (when dec
				    (let ((places (- end p)))
				      (setq pos end)
				      (return-from next-token
					(new-fraction number dec places)))))
				(setq pos p)
				(return-from next-token
				  (values number number-index)))
			   (t (when (or (not id-allows-start-digit)
					(not (identifier-continue-char-p c)))
				(setq pos end)
				(return-from next-token
				  (values number number-index))))))))))
	       ;; recognize a grammar keyword
	       (multiple-value-bind (token-association token-length)
		   (recognize-kwd string pos string-ln actionv find-id?)
		 (when token-association
		   ;; token recognized
		   (setq pos (+ pos token-length)
			 token (car token-association))
		   (return-from next-token
		     (values token (cdr token-association)))))
	       ;; recognize an identifier or string
	       (setf find-string? (and string-index
				       (find string-index actionv
					     :key #'car)))
	       (when (or find-id? find-string?)
		 (let ((char (schar string pos)) (c #\space))
		   (declare (character char c))
		   (flet 
		       ((parse-delimited-id (delimiter symb?)
			  (block parse-delimited-id
			    ;; when successful set token and pos!!
			    (flet ((eof-error ()
				     (return-from read-parser
				       (funcall
					error-fn
					(format
					 nil "Closing ~:[String~;Symbol~] delimiter ~S expected"
					 symb? delimiter)))))
			      (when (char= char delimiter)
				(do ((p (incf pos) (1+ p))
				     (escaped? nil (char= c #\\)))
				    (nil)
				  (declare (fixnum p))
				  (when (= p string-ln)
				    (if more-fn
					(setq string
					      (concatenate
					       'string
					       string (string #\Newline)
					       (funcall more-fn #'eof-error))
					      string-ln (length string))
				      (eof-error)))
				  (setq c (schar string p))
				  (when (and (char= c delimiter)
					     (not escaped?))
				    (setq token (subseq string pos p)
					  pos (1+ p))
				    (return-from parse-delimited-id t))))))))
		     (and find-id?
			  (parse-delimited-id symbol-delimiter t)
			  (return-from next-token
			    (values (intern token) identifier-index)))
		     (and find-string?
			  (parse-delimited-id string-delimiter nil)
			  (return-from next-token
			    (values token string-index))))

		   ;; Does char start an identifier?
		   (unless find-id? (funcall error-fn (unrecognized-token-error
						       string pos actionv grammar)))
		   (flet ((parse-id ()
			    ;; Any char not in *identifier-continue-chars* terminates
			    (do ((p (1+ pos) (1+ p))) 
				((or (= p string-ln)
				     (not (identifier-continue-char-p (schar string p))))
				 (prog1 (if *preserve-case*
					    (subseq string pos p)
					  (upcased-subseq string pos p))
				   (setq pos p)))
			      (declare (fixnum p)))))
		     (let ((Id-String
			    (block Identifier
			      (when (identifier-start-char-p char)
				(let ((id1 (parse-id)))
				  (when (or (= pos string-ln)
					    (char/= (schar string pos) #\:)
					    *disallow-packages*)
				    (return-from Identifier id1))
				  ;; more chars follow the ":" ?
				  (let ((package (find-package id1)))
				    (unless package
				      (return-from Identifier id1))
				    ;; <package-symbol>: ...
				    (let* ((p (1+ pos))
					   (next (schar string p)))
				      (when (char= next #\:)
					(setq next (schar string (incf p))))
				      (unless (identifier-start-char-p next)
					(return-from Identifier id1))
				      (setq pos p)
				      (return-from next-token
					(values
					 (intern (the simple-string (parse-id)) package)
					 identifier-index)
					)))))
			      ;; Symbol in keyword package ?
			      (when (and (char= char #\:)
					 (identifier-start-char-p
					  (schar string (incf pos))))
				(return-from next-token
				  (values (intern (the simple-string
						       (parse-id))
						  *keyword-package*)
					  identifier-index))))))
		       (when Id-String
			 (return-from next-token
			   (values (if intern-identifier
				       (intern Id-String) Id-String)
				   identifier-index)))))))
	       (if (and junk-allowed
			(find end-symbol-index actionv :key #'car))
		   (return-from next-token (values nil end-symbol-index))
		 ;; none of the symbols that we are looking for found
		 (funcall error-fn (unrecognized-token-error
				    string pos actionv grammar))))))
	(lr-parse
	 (if-debugging-lexer		;  for testing
	  #'(lambda (a)
	      (multiple-value-bind (token id)
		  (next-token a)
		(format t "~%New Token: ~S . ~S Pos: ~S"
			token id pos)
		(values token id)))
	  #'next-token)
	 ;; This is the error function supplied to the parsing engine:
	 #'(lambda (msg)
	     (when print-parse-errors
	       (format t "~%Last token read: ~S~%Remaining: ~A~@[~A ...~]~%"
		       token
		       (subseq string pos)
		       (when more-allowed (funcall more-fn))))
	     (funcall error-fn msg))
	 grammar
	 junk-allowed
	 #'(lambda () last-pos))))))

;----------------------------------------------------------------------------;
; recognize-kwd
;--------------
; 
(defun recognize-kwd (string pos string-length actionv find-id?)
  ;; Does any of the terminal symbols of the grammar start STRING at POS?
  ;; In case it does, it must be the longest one
  ;; the ordering of terminal-alist makes sure we find the longest keyword
  ;; first
  (declare (string string) (fixnum string-length))
  (let ((max-token-length (- string-length (the integer pos))))
    (declare (fixnum max-token-length))
    (flet ((recognize-kwd-aux (ta)
	     (do ((ta-rest ta (cdr (the cons ta-rest))))
		 ((null ta-rest) nil)
	       ;; (break "recognize-kwd: ~s ~%~s" actionv ta-rest)
	       (let ((token-association (car (the cons ta-rest))))
		 (when (find (cdr token-association) actionv :key #'car)
		   ;; search only for a legitimite keyword
		   (let* ((terminal-token (car token-association))
			  (token-length (length (the string terminal-token))))
		     (declare (fixnum token-length) (string terminal-token))
		     (and (>= max-token-length token-length)
			  (let ((string-end (+ pos token-length)))
			    (declare (fixnum string-end))
			    ;; (break "recognize-kwd 2: ~s ~%~s" terminal-token string)
			    (and (if *case-sensitive*
				     (string= terminal-token string
					      :start2 pos :end2 string-end)
				   (string-equal terminal-token string
						 :start2 pos :end2 string-end))
				 ;; 
				 ;; If we recognize a keyword, that could start
				 ;; an identifier, the following char must
				 ;; not also be a symbol-continue-char.
				 ;; If it is (e.g. "agent1") and there exists
				 ;; no shorter key that would accept this,
				 ;; then we will not recognize the key ("agent")
				 ;; but this leads us to recognize in "?u?x" the
				 ;; token "?u?" instead of "?"
			       
				 ;; if we are at the end of the string,
				 ;; we accept
				 (or (not find-id?)
				     (not (< string-end string-length))
				     ;; if a identifier-continue-char doesn't
				     ;; follow, we also accept
				     (not (identifier-continue-char-p
					   (schar string string-end)))
				     ;; if the key does not start with
				     ;; an identifier-start-char we accept
				     (not (identifier-start-char-p
					   (schar terminal-token 0)))
				     ;; if any of the remaining chars of the key
				     ;; is not a identifier-continue-char,
				     ;; we also accept
				     (find-if-not #'identifier-continue-char-p
						  terminal-token
						  :start 1))))
			  (return (values token-association token-length)))))))))
      (recognize-kwd-aux
       (svref *terminal-alist-SEQ*
	      (char-code
	       (if *case-sensitive*
		   (the character (schar string pos))
		 (char-downcase (the character (schar string pos))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                file-parser 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse expressions in GRAMMAR reading from FILE
;; returns: a list of the parse-results, i.e. what would have been 
;;          returned by read-parser

(defvar *comment-brackets* '(("#|" . "|#")) )
(defvar *comment-start* #\; )

(defun file-parser (file &key 
			 (error-fn #'error)
			 (print-parse-errors t)
			 (grammar *current-grammar*)
			 (verbose *load-verbose*))
  (with-open-file (s (merge-pathnames file) :direction :input)
    (file-parser-aux s error-fn print-parse-errors grammar verbose)))

(defun file-parser-aux (stream error-fn print-parse-errors grammar verbose
			 &aux R (eof (cons nil nil)))
  (labels ((skip-lines (stream end)
	     ;; ignore lines until end is found
	     (let ((l (read-line stream nil eof)))
	       (if (stringp l)
		   (let ((p (search end l)))
		     (if p
			 (let ((l-rest (string-left-trim
					'(#\Space #\Tab)
					(subseq l (+ p (length end))))))
			   (if (string= l-rest "")
			       (next-line stream)
			     l-rest))
		       (skip-lines stream end)))
		 l)))
	   (next-line (stream)		; ignore comments
	     (let ((l (read-line stream nil eof)))
	       (when verbose (terpri) (princ l))
	       (if (stringp l)
		   (let ((l-length (length (setq l (string-left-trim
						    '(#\Space #\Tab) l)))))
		     (if (zerop l-length)
			 (next-line stream)
		       (if (char= *comment-start* (schar l 0))
			   (next-line stream)
			 ;; does this line start a comment
			 (dolist (comment *comment-brackets* l)
			   (let* ((start (car comment))
				  (start-length (length start)))
			     (when (and
				    (>= l-length start-length)
				    (string= l start :end1 start-length))
			       ;; a comment found
			       (return
				 (setq l (skip-lines
					  stream
					  (cdr comment))))))))))
		 l))))
    (do ((line (next-line stream)))
	((eq line eof) (nreverse R))
      (multiple-value-bind (expr rest)
	  (read-parser line
		       :error-fn error-fn
		       :print-parse-errors print-parse-errors
		       :grammar grammar
		       :junk-allowed t
		       :more-allowed t
		       :more-fn #'(lambda (&optional error-fn)
				    (setq line (next-line stream))
				    (if (eq line eof)
					(if error-fn
					    (funcall error-fn)
					  (error "Reached end of file ~S while parsing"
					       stream))
				      line)))
	;; (when verbose (let ((*print-structure* t)) (print expr)))
	(push expr R)
	(when (eq line eof) (return (nreverse R)))
	(setq line (if rest
		       (subseq line rest)
		     (next-line stream)))))))

;----------------------------------------------------------------------------;
; debug-parser
;-------------
; 
; 
(defun debug-parser (&key (grammar t) (lexer nil))
  (setq *grammar-debug* grammar
	*lexer-debug* lexer)
  (let ((*default-pathname-defaults*
	 (if (or grammar lexer)
	     (merge-pathnames
	      *ZEBU-directory*
	      (make-pathname :type (first *load-source-pathname-types*)))
	   (merge-pathnames
	    (make-pathname
	     :type (first *load-binary-pathname-types*))
	    *ZEBU-binary-directory*))))
    ;; "zebu-loader" needs only to be loaded if the compiler
    ;; in-line codes slot accessors and does not keep the function
    ;; definitions
    #+ALLEGRO (load "zebu-loader")
    (load "zebu-driver")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             End of zebu-driver.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
