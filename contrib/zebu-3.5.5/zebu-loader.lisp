; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-loader.l
; Description:  load a ZEBU grammar table
; Author:       Joachim H. Laubsch
; Created:       6-Nov-90
; Modified:     Thu Oct  2 12:00:10 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-loader.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-loader.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
; 13-Jan-93 (Joachim H. Laubsch)
;  implemented terminal-alist access via the vector terminal-alist-SEQ
;  5-Nov-91 (Joachim H. Laubsch)
;  removed dependency on LUCID in the use of backquoted expressions
;  in semantic actions
; 16-Jul-91 (Joachim H. Laubsch)
;  Added a facility to deal with multiple grammars
;  lr-parse takes a third argument, a grammar
;  READ-PARSER and LIST-PARSER take a :grammar keyword argument, defaulting to
;  *current-grammar*
;  in order to use several grammars we need several 
;    *IDENTIFIER-CONTINUE-CHARS*, *IDENTIFIER-START-CHARS*
;    
;  1-Mar-91 (Joachim H. Laubsch)
;  did monitoring, found that 75% of the time is in the lexer.
;  rewrote ZEBU::RECOGNIZE-TOKEN to use a hashtable of terminal-tokens
;  this sped up this function by a factor of 35. Speed-up of READ-PARSER: 3.5
; 11-Dec-90 (Joachim H. Laubsch)
;  Introduce the ZEBU package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
(provide "zebu-loader")

;;; The following data structures are loaded from a parse table file by the 
;;; function which follows.
;;;
;;; lexicon is a vector of strings or lisp symbols , indexed by the 
;;; "grammar symbol indices",  which are the instantiations of
;;; the grammar symbols.
;;;
;;; terminal-indices is a list of the grammar symbol indices indicating
;;; which among them are terminal symbols.
;;;
;;; production-info is a vector, indexed by the production indices.
;;; Each item is a cons: the cars index the symbols
;;; which are the lhs of the productions, the cdrs indicate the
;;; lengths of the productions.
;;;
;;; action-table is a vector indexed by the state indices.
;;; Each state's entry is a vector whose elements represent
;;; defined entries in the action parsing function. These entries are 3 element
;;; lists whose first elements are the indices of the grammar symbol argument
;;; to the action parsing function.  The second elements in the lists are an
;;; encoding of the action function: 's for shift, 'r for reduce, 'a for
;;; accept.  The third elements are production or next state indices as
;;; approprite.  The three element lists appear in their surrounding
;;; vectors sorted on their cars.
;;;
;;; goto-table is arranged similar to action-table but has two element
;;; lists instead of three.  The second element of each list are the
;;; index of the state to goto.
;;; 
;;; end-symbol-index holds the index of the end symbol.
;;;
;;; terminal-alist associates terminal symbol instantiations with
;;; their indices.
;;;
;;; client-lambdas are a vector of procedures, indexed by production index,
;;; which correspond to productions in the grammar.  The client lambdas are 
;;; what the parser calls to do syntax directed something by side effect.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Zebu Grammar Struct
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helps define the function that computes whether a character can continue 
;; a symbol

(defvar *identifier-continue-chars*
  "$-_*.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890?"
  "Characters that may occur in an identifier. Set this before calling zebu-load-file.")

(defvar *identifier-start-chars* "$-*?abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Characters that may start an identifier.")

;----------------------------------------------------------------------------;
; *string-delimiter*, *symbol-delimiter*
;---------------------------------------
;; An NLL-constant can now be a string or a symbol. A string is surrounded
;; by double-quotes (#\"), as in: P(arg1: \"|Jon Doe's Deli|\")
;; A symbol is surrounded by single-quotes (#\'), as in: 
;;     P(arg1: 'Jon Doe')
;; or  P(arg1: '|Jon Doe|')
;; By default, the single-quotes may be omitted at parsing in case the
;; symbol contains only characters which are in
;;    (grammar-identifier-continue-chars *current-grammar*)
;; as in P(arg1: Jon_Doe)
;; Either set these variables before the grammar is loaded
;;   or   supply the initial values explicitely in the .grm file
;;        e.g. (:name "nll" :string-delimiter #\" :symbol-delimiter #\')
(defvar *string-delimiter* #\"
  "Delimits a lexical token, considered as a STRING.")

(defvar *symbol-delimiter* #\'
  "Delimits a lexical token, considered as a SYMBOL.")

(defvar *preserve-case* nil
  "If true, the case of an identifier will be preserved (default false).")

(defvar *case-sensitive* nil
  "If true, the case of a keyword matters otherwise case is ignored when \
looking for the next token (default false).")

(defvar *disallow-packages* nil
  "If false, Zebu parses identifiers as symbols possibly qualified by a package")
    
;----------------------------------------------------------------------------;
; grammar
;--------
; 
(defstruct (grammar (:print-function print-grammar))
  name
  lexicon
  terminal-indices
  production-info
  action-table
  goto-table
  lr-parser-start-state-index
  end-symbol-index
  client-lambdas
  identifier-index
  string-index
  (number-index nil)
  (identifier-continue-chars     *identifier-continue-chars* :type string)
  (identifier-continue-chars-V   (make-array char-code-limit :element-type 'bit
					     :initial-element 0))
  (identifier-start-chars        *identifier-start-chars* :type string)
  (identifier-start-chars-V      (make-array char-code-limit :element-type 'bit
					     :initial-element 0))
  ;; a vector to be indexed by the char-code of the first character of a key
  ;; each element contains an alist of pairs: (,terminal-token . ,index)
  (terminal-alist-SEQ            (make-sequence 'vector
						char-code-limit
						:initial-element nil))
  (case-sensitive                *case-sensitive*)
  (string-delimiter              *string-delimiter* :type character)
  (symbol-delimiter              *symbol-delimiter* :type character)
  file
  (package                       *package*)
  grammar				; the grammar used to parse the
					; grammar being defined
					; defaults to the null-grammar
					; but you can use the meta-grammar
  (zb-rules ())
  (domain ())
  domain-file
  (lex-cats ())				; an alist of cateory name and
					; regular expressions
  (lex-cat-map ())			; an alist of index and reg-ex function
  (white-space                  '(#\Space #\Newline #\Tab))
  (intern-identifier            t)	; Identifier is represented as symbol
  (id-allows-start-digit        nil)    ; An Identifier may start with a digit
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Null Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *NULL-Grammar* (make-grammar :name "null-grammar"))

(defun print-grammar (item stream level)
  (declare (ignore level))
  (format stream "<Zebu Grammar: ~A>" (grammar-name item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             register a grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *all-grammars*
  (list (cons (grammar-name *NULL-Grammar*) *NULL-Grammar*)))

(defun find-grammar (name)
  (cdr (assoc (string name) *all-grammars* :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Lexical Analysis Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *identifier-continue-chars-V*)
(declaim (inline identifier-continue-char-p))
(defun identifier-continue-char-p (char)
  (declare (character char))
  (= 1 (sbit *identifier-continue-chars-V* (char-code char))))

(defvar *identifier-start-chars-V*)
(declaim (inline identifier-start-char-p))
(defun identifier-start-char-p (char)
  (declare (character char))
  (= 1 (sbit *identifier-start-chars-V* (char-code char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Lex-Cats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline add-to-lex-cat-map))
;; preserve the order of the definition

;; from marc@rose.de
;; the (index . (symbol-function terminal-token)) will be inserted 
;; according to the order in *lex-cats* = (grammar-lex-cats grammar)

(defun add-to-lex-cat-map (index terminal-token grammar
				 &aux (lower (cdr (member terminal-token
							  (grammar-lex-cats grammar)
							  :key #'car))))
  (setf (grammar-lex-cat-map grammar)
        (merge 'list (list (cons index (symbol-function terminal-token)))
               (grammar-lex-cat-map grammar)
               #'(lambda (&rest r)
		   (not (member (car r) lower
				:key #'(lambda (x)
					 (symbol-function (car x))))))
               :key #'cdr)))

;(defun add-to-lex-cat-map (index terminal-token grammar)
;  (setf (grammar-lex-cat-map grammar)
;	(nconc (grammar-lex-cat-map grammar)
;	       (list (cons index (symbol-function terminal-token))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Debugging 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *grammar-debug* nil
  "If TRUE at compile or load time, the parser emits traces, else not.")

(defmacro if-debugging (&rest x)
  `(progn . ,(if *grammar-debug*
		 x
	       'nil)))

(eval-when (compile)
  (setq *grammar-debug* nil))

#||
(eval-when (eval)
  (setq *grammar-debug* T))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Read in a set of parse tables as written by (dump-tables) .
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zebu-load-file (filename &key (verbose t)
		       &aux lexicon terminal-indices (*package* *package*))
  ;; returns a grammar and registers this grammar on *all-grammars*
  (let ((path (probe-file (merge-pathnames
			   filename
			   (merge-pathnames (make-pathname :type "tab")))))
	(*load-verbose* verbose))
    (if path
	(when verbose
	  (format t "~%Loading ~s" (namestring path)))
      (error "File not found: ~S" filename))
    (unless (equalp (pathname-type path) "tab")
      (let ((name (pathname-name path)))
	(cerror "~S is now compiled."
		"~S is not a Zebu output!~%;;; Compile ~S first!"
		name filename name)
	(setf path (merge-pathnames (make-pathname :type "tab") path)
	      filename (namestring path))))
    (with-open-file (port path :direction :input)
      (let ((options			; 1: read grammar-options
	     (catch 'read-grammar-options
	       (check-grammar-options (read port) path nil))))
	(unless options
	  (close port)
	  (setq port (open path :direction :input))
	  (setq options
		(catch 'read-grammar-options
		  (check-grammar-options (read port) path nil))))
	(let* ((g (apply #'make-grammar options))
	       (terminal-alist-SEQ (grammar-terminal-alist-SEQ g)))
	  (declare (type grammar g))
	  (prepare-domain (grammar-domain g))
	  ;; 1a: load the domain file
	  (let ((grammar-domain-file (grammar-domain-file g)))
	    (when grammar-domain-file
	      (let ((grammar-domain-file-name
		     (pathname-name (pathname grammar-domain-file))))
		(or (block find-domain-file
		      (dolist (type (append *load-binary-pathname-types*
					    *load-source-pathname-types*))
			(dolist (domain-path (list path (grammar-file g)))
			  (let ((domain-file 
				 (merge-pathnames
				  (make-pathname 
				   :name grammar-domain-file-name
				   :type type)
				  domain-path)))
			    (when (probe-file domain-file)
			      (when *load-verbose*
				(format t "~%Loading domain file ~s"
					(namestring domain-file)))
			      (return-from find-domain-file
				(load domain-file)))))))
		    (warn "No domain file found")))))

	  ;; 2: read grammar-lexicon
	  (setf (grammar-lexicon g)          (setf lexicon (read port))
		;; 3: read grammar-terminal-indices
		(grammar-terminal-indices g) (setf terminal-indices (read port))
		;; 4: read grammar-production-info
		(grammar-production-info g)  (read port))
	  (let ((old-grammar (assoc (grammar-name g) *all-grammars*
				    :test #'string=)))
	    (if old-grammar
		(setf (cdr old-grammar) g)
	      (setf *all-grammars* (acons (grammar-name g) g *all-grammars*))))
	
	  ;; 5: read grammar-action-table
	  (setf (grammar-action-table g) 
		(vectorize-vector-of-lists (read port)))
	
	  ;; 6: read grammar-goto-table
	  (setf (grammar-goto-table g) (vectorize-vector-of-lists (read port))
		;; 7: read grammar-lr-parser-start-state-index
		(grammar-lr-parser-start-state-index g) (read port)
		;; 8: read grammar-end-symbol-index
		(grammar-end-symbol-index g) (read port)
		;; 9: read grammar-client-lambdas
		(grammar-client-lambdas g) (read-parser-actions port g))

	  ;; IDENTIFIER-START-CHARS
	  (let ((identifier-start-chars-V
		 (grammar-identifier-start-chars-V g))
		(identifier-start-chars (grammar-identifier-start-chars g)))
	    (dotimes (i (length identifier-start-chars))
	      (let ((c (schar identifier-start-chars i)))
		(declare (character c))
		(setf (sbit identifier-start-chars-V (char-code c))
		      1)
		(when (digit-char-p c)
		  (setf (grammar-id-allows-start-digit g) t)))))

	  ;; IDENTIFIER-CONTINUE-CHARS
	  (let ((identifier-continue-chars-V
		 (grammar-identifier-continue-chars-V g))
		(identifier-continue-chars
		 (grammar-identifier-continue-chars g)))
	    (dotimes (i (length identifier-continue-chars))
	      (setf (sbit identifier-continue-chars-V
			  (char-code
			   (the character
				(schar identifier-continue-chars i))))
		    1)))

	  ;; sort the terminal-alist so that terminals with the same
	  ;; initial string are sorted by decreasing length
	  ;; i.e. if "?" and "?u?" are both terminals, then "?u?"
	  ;; should be found first.
	  ;; This can simply be achieved by sorting according to 
	  ;; ascending key length.
	  (dotimes (i (length (the simple-vector terminal-indices)))
	    (let* ((index (svref terminal-indices i))
		   (terminal-token (svref lexicon index)))
	      (declare (type (or symbol string) terminal-token))
	      (typecase terminal-token
		(string
		 (let ((char1-code
			(char-code (let ((c (schar terminal-token 0)))
				    (declare (character c))
				    (if (grammar-case-sensitive g)
					c
				      (char-downcase c)))))
		       (token-association `(,terminal-token . ,index)))
		   ;; keep a table indexed by char-code of first-char
		   ;; of the terminal tokens
		   (let ((bucket (elt terminal-alist-SEQ char1-code)))
		     (setf (elt terminal-alist-SEQ char1-code)
			   (if bucket
			       (sort (cons token-association bucket)
				     #'(lambda (a b) (declare (string a b))
					       (> (length a) (length b)))
				     :key #'car)
			     (list token-association))))))
		(symbol
		 (let ((terminal-token-name (symbol-name terminal-token)))
		   (declare (string terminal-token-name))
		   (cond ((string= terminal-token-name "IDENTIFIER")
			  (setf (grammar-identifier-index g) index))
			 ((string= terminal-token-name "STRING")
			  (setf (grammar-string-index g) index))
			 ((string= terminal-token-name "NUMBER")
			  (setf (grammar-number-index g) index))
			 ;; for lexical categories: remember index
			 ((assoc terminal-token (grammar-lex-cats g))
			  (add-to-lex-cat-map index terminal-token g))
			 (t (warn "If ~S is a terminal it should be a string, not a symbol.~%If it's a non-terminal it's undefined."
				  terminal-token))))))))
	  g)))))

(defun read-parser-actions (port grammar)
  ;; zb-rules = [(<NT> . <zb-rule>) ...]
  (let ((zb-rules (read port))
	(actions  (make-sequence
		   'vector
		   (length (grammar-production-info grammar))))
	(actions-idx 1))
    (setf (svref actions 0) :PLACE-HOLDER)
    (dotimes (i (length zb-rules))
      (let ((pair (svref zb-rules i)))
	(let ((zb-rule (cdr pair)))
	  (dolist (prod (zb-rule--productions zb-rule))
	    (let ((action (production-rhs--build-fn prod)))
	      (setf (svref actions actions-idx)
		    (if (symbolp action)
			(if (or (eq action 'identity) (null action))
			    nil
			  (if (fboundp action)
			      (symbol-function action)
			    (progn
			      (warn "At parse time, ~S should be defined."
				    action)
			      action)))
		      action
		      ;; (if (fboundp 'compile)
		      ;;    (compile nil action)
		      ;;   (eval `(function ,action)))
		      ))
	      (incf actions-idx))))))
    (setf (grammar-zb-rules grammar) zb-rules)
    actions))

(defun vectorize-vector-of-lists (V  &aux alist)
  (declare (simple-vector V) (dynamic-extent alist))
  (dotimes (i (length V) V) 
    (let* ((sublist (svref V i))
	   (pair (assoc sublist alist :test #'equal)))
      (if pair
	  (setf (svref v i) (cdr pair))
	(let ((subV (list->vector sublist)))
	  (setf (svref v i) subV)
	  (push (cons sublist subV) alist))))))


;----------------------------------------------------------------------------;
; load-from-command-line (for UNIX)
;----------------------------------
; Load a compiled grammar from a command line argument:
;    Zebu-Parser ex1.tab
; Zebu-Parser <comiled-grammar> -l <file to load before grammar>
;             -e "<form to be evaluated>"
;             -quit 
#+LUCID
(defun load-from-command-line ()
  (let ((*default-pathname-defaults*
	 (make-pathname :directory
			(pathname-directory (working-directory))))
	(help "Zebu-Parser [-zb] <compiled-grammar> [-l <file>]*
  [-e <form to eval>]*  [-quit]"))
    (handler-case
     (do* ((i 1 (1+ i))
	   (arg (command-line-argument i) (command-line-argument i))
	   (val (command-line-argument (1+ i))
		(command-line-argument (1+ i))))
	  ((null arg)
	   (when (= i 1)
	     (progn (warn "~a" help) (quit))))
       ;;(format t "~%arg: ~s val: ~s" arg val)
       (cond ((equal arg "-l")
	      (incf i) (load val))
	     ((equal arg "-e")
	      (incf i) (eval (read-from-string val)))
	     ((equal arg "-quit") (quit))
	     ((equal arg "-h")
	      (format t "~%~a" help))
	     ((equal arg "-zb")
	      (incf i) (zebu-load-file val :verbose t))
	     (t (if (probe-file arg)
		    (zebu-load-file arg :verbose t)
		  (progn
		    (warn "Unrecognized argument ~S~%~a" arg help)
		    (quit))))))
     (error (c)
	    (format t "~&Zebu-Parser failed: ~A~%" c)
	    (quit)))))

;----------------------------------------------------------------------------;
; zebu-load-top
;--------------
; interactive loader invocation
; 
(defun zebu-load-top ()
  (format t "~&Enter the name of a Zebu .tab file to load: ")
  (let ((ifile (read-line t)))
    (zebu-load-file ifile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          End of zebu-loader.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
