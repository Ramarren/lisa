; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-loadgram.l
; Description:  Load a grammar file (type: .zb) so that it can be compiled
; Author:       Joachim H. Laubsch
; Created:      10-Oct-90
; Modified:     Thu Oct  2 16:31:15 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-loadgram.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1990, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-loadgram.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
; 26-Jul-94 (Joachim H. Laubsch)
;  Fixed Bug with "." as separator (ambiguous constituent names were made)
; 12-Mar-93 (Joachim H. Laubsch)
;  Bind a Kleene* Variable
;  9-Mar-93 (Joachim H. Laubsch)
;  allow a print-function specification in a domain definition rule
;  8-Feb-93 (Joachim H. Laubsch)
;  allow defstruct forms for domain definition among the rules
; 31-Jul-92 (Joachim H. Laubsch)
;  Introduced Kleene * and +
; 24-Apr-92 (Joachim H. Laubsch)
;  Introduced a meta-grammar for reading a user grammar
;  The meta-grammar is compiled using the null-grammar
; 25-Mar-92 (Joachim H. Laubsch)
;  Warn about unused non-terminals
; 16-Jul-91 (Joachim H. Laubsch)
;  to deal with multiple-grammars, first find in a grammar file: *GRAMMAR-OPTIONS*
;  a keyworded arglist that can be passed to MAKE-GRAMMAR
; 20-Mar-91 (Joachim H. Laubsch)
;  Introduced error checking during loading of grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Copyright (C) 1989, by William M. Wells III
;;;                         All Rights Reserved
;;;     Permission is granted for unrestricted non-commercial use.

(IN-PACKAGE  "ZEBU")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *Kleene+-rules* ()
  "A list of rules that are generated as a consequence of the Kleene notation")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Read in a File Containing a Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RULES
;;; About the representations of grammars in files:
;;;      non terminals are represented by lisp symbols,
;;;      terminals by symbols (IDENTIFIER NUMBER STRING), or strings
;;; for example then BNF rule:
;;;         A ::= B | C | "foo" | "c" | <the-empty-string>
;;;
;;; would be encoded -- using the NULL Grammar -- as:

;;; (defrule A := B
;;;            :build (f1 B)
;;;
;;;            := C
;;;            :build (f2 C)
;;;
;;;            := "foo"
;;;                                   ; ommitting the build clause has the
;;;            := "c"                 ; effect of calling the identity function
;;;            := () )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Format for a grammar file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||
A grammar file has a filename of type "zb".

The file consists of:

1.  A keyword agument-list for MAKE-GRAMMAR.
    Example:
        (:name "pc2"
         :package "CL-USER"
	 :grammar "zebu-mg"
	 :identifier-continue-chars
	 "$-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
	 )
2.  If parsing with the NULL-Grammar
     ( the default, if no :grammar keyword is given in 1.)
     one or more defrule forms as above
    If parsing with the META-Grammar
     one or more rules using the syntax of the Meta-grammar
     The start symbol of the grammar will be the lhs of the first
     production encountered.  

The symbol AUGMENTED-START is reserved and will automatically appear in
a production deriving the start symbol.
The symbol THE-EMPTY-STRING is also reserved. 

Use load-grammar to internalize a grammar in the above syntax.
 *productions* holds a list of all the productions.
 *lambdas* holds a list of all of the associated lambdas (in reverse order)
 *non-terminals* holds a list of all the non-terminals.
Each non-terminal symbol has a list of the productions it
appears in the left hand side of under its own-productions
property.
*g-symbol-alist* holds an alist whose cars are the string or symbol
  which is read from the grammar, and whose cdrs hold corresponding
  g-symbol structures; the order is in the reverse sense of *symbol-array*.
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *symbols*) ; a list of the grammar symbols
(defvar *symbol-array*) ; indexed by the symbol's index, of g-symbols
(defvar *productions*)
(defvar *production-count*)
(defvar *g-symbol-count*)
(defvar *g-symbol-alist*)
(defvar *start-symbol*)
(defvar *empty-string-g-symbol*)
(defvar *augmented-start-g-symbol*)
(defvar *the-end-g-symbol*)

(defvar *grammar-options*)

(declaim (special
          *identifier-continue-chars*
          *identifier-start-chars*
          *null-grammar*
          *compiler-grammar* 
          *domain-type-hierarchy*
          *domain-types*
          *domain-structs*
          *lex-cats*))

;; new rule format
(defvar *ignore* '("DUMMY" "DUMMY1" "DUMMY2" "DUMMY3" "DUMMY4"
		   "DUMMY5" "DUMMY6" "DUMMY7" "DUMMY8"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro post-inc (x)
  `(let ((old ,x))
     (setq ,x (1+ ,x))
     old))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Initialisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-grammar ()
  (setq *symbols* '()
	*productions* '()
	*production-count* 0
	*g-symbol-count* 0
	*g-symbol-alist* '()
	*start-symbol* '()
	*zb-rules* '()
	*lex-cats* '()
	*domain-types* '()
	*domain-structs* '()
	*domain-type-hierarchy* '()
	*empty-string-g-symbol* (g-symbol-intern 'the-empty-string)
	*augmented-start-g-symbol* (g-symbol-intern 'augmented-start)
	*the-end-g-symbol* (g-symbol-intern 'the-end-g-symbol))
  )

;----------------------------------------------------------------------------;
; g-symbol-intern
;----------------
; This is sort of like interning.  returns a g-symbol. 
; if (equal (string x) (string y)) the g-symbols are eq

(defun g-symbol-intern (string-or-symbol)
  (check-type string-or-symbol (or string symbol)
	      "a string or symbol, in order to be a well-formed Zebu grammar rule.")
  (let ((pair (assoc string-or-symbol *g-symbol-alist*
		     :test #'equal)))
    (if pair
	(cdr pair)
      (let ((symbol (new-g-symbol
		     (string string-or-symbol)
		     (post-inc *g-symbol-count*))))
	(push (cons string-or-symbol symbol) *g-symbol-alist*)
	(push symbol *symbols*)
	symbol))))

;;; Do various things, fixing up global data structures and
;;; fields of grammar symbols.  A bit sleazy: *start-symbol* being NIL
;;; is used to detect the first production.

(defun process-production (lhs rhs &optional internal-use?)
  (let ((lhs-symbol  (g-symbol-intern lhs))
	;; intern constituent as a G-SYMBOL
	(rhs-symbols (mapcar #'g-symbol-intern rhs)))
    (unless *start-symbol*
      (setq *start-symbol* lhs-symbol)
      (format t "~%Start symbols is: ~A~%" 
	      (g-symbol-name *start-symbol*))
      (process-production 'AUGMENTED-START (list lhs) t))
    (let ((production
	   (make-production :lhs lhs-symbol
			    :rhs rhs-symbols
			    :production-index (post-inc *production-count*)
			    :production-length (length rhs-symbols))))
      (when (and (eq lhs-symbol *augmented-start-g-symbol*)
		 (not internal-use?))
	(error "AUGMENTED-START is a reserved grammar symbol"))
      (push production *productions*)
      (g-symbol-add-production lhs-symbol production)
      (let ((rhs-symbol-set (make-oset :order-fn #'g-symbol-order-function)))
	(dolist (gs rhs-symbols)
	  (oset-insert! gs rhs-symbol-set))
	(dolist (gs (oset-item-list rhs-symbol-set))
	  (push production (g-symbol-rhs-productions gs)))))))

;----------------------------------------------------------------------------;
; pre-process-rules
;------------------
; Given a function to produce the next rule, process it
; and warn about:
; - redefinition of lhs symbol
; - repeated equal rhs
; - unused lhs symbols
; - undefined non-terminals
; - duplicate constituents

(defun pre-process-rules (next-rule-fn do-semantics? &aux non-terminals)
  (do ((zb-rule (funcall next-rule-fn) (funcall next-rule-fn)))
      ((null zb-rule))
    (let ((lhs (zb-rule--name zb-rule)))
      (when (assoc lhs *zb-rules*)
	(warn "Non-terminal ~S is defined again" lhs)
	;; (break "Rule: ~S" zb-rule)
	)
      (push (cons lhs zb-rule) *zb-rules*)
      (do ((prods (zb-rule--productions zb-rule) (cdr prods)))
	  ((null prods))
	(let* ((production-rhs (car prods))
	       (syntax (production-rhs--syntax production-rhs))) 
	  (when (member syntax (rest prods)
			:test #'equal :key #'production-rhs--syntax)
	    (warn "Multiply defined rhs of rule for ~S: ~S" lhs syntax))
	  (when (member "" syntax :test #'equal)
	    (warn "Empty keyword ignored in rhs of ~s:~{ ~s~}" lhs syntax)
	    (setf syntax
		  (setf (production-rhs--syntax production-rhs)
			(delete "" syntax :test #'equal))))
	  (expand-Kleene-constituent production-rhs)))
      (do ((prods (zb-rule--productions zb-rule) (cdr prods)))
	  ((null prods))
	(let* ((production-rhs (car prods))
	       (syntax (production-rhs--syntax production-rhs))
	       syntax1)			; the <NT>.<digit> notation is removed
	  ;; remove the  <NT>.<digit> notation from the rhs
	  (do ((rhs-tail syntax (cdr rhs-tail))) ((null rhs-tail))
	    (let ((constituent (car rhs-tail)))
	      (typecase constituent
		(symbol
		 (when (and (production-rhs--semantics production-rhs)
			    (member constituent (cdr rhs-tail)))
		   (warn "Duplicate constituent in RHS of ~S~% ~S~% Use <NT>.<digit>"
			 lhs syntax))
		 (let ((cname (constituent-name constituent)))
		   (push cname syntax1)
		   (pushnew cname non-terminals)))
		(T (push constituent syntax1)))))
	  (when do-semantics? (process-semantics production-rhs))
	  (process-production lhs (nreverse syntax1))))))
  (let* ((lhs-non-terminals (nreverse (mapcar #'car *zb-rules*)))
	 (lexical-categories (mapcar #'(lambda (c)
					 (symbol-name (car c)))
				     *lex-cats*))
	 (rhs-non-terminals 
	  (set-difference non-terminals
			  (union *open-categories* lexical-categories)
			  :test #'string-equal :key #'string))
	 (undefined-non-terminals (set-difference rhs-non-terminals
						  lhs-non-terminals))
	 (unused-non-terminals (set-difference (cdr lhs-non-terminals)
					       ;; the start symbol does not
					       ;; have to occur on any rhs
					       rhs-non-terminals))
	 (unused-lex-cats
	  (set-difference lexical-categories non-terminals
			  :test #'string= :key #'string))
	 (overused-lex-cats
	  (intersection lhs-non-terminals lexical-categories
			:test #'string= :key #'string)))	   
    (when undefined-non-terminals
      (warn "The following non-terminals had no definition:~% ~{~a ~}"
	    undefined-non-terminals))
    (when unused-non-terminals
      (warn "The following non-terminals where defined but not used:~% ~{~a ~}"
	    unused-non-terminals))
    (when unused-lex-cats
      (warn "The following lexical categories where defined but not used:~% ~{~a ~}"
	    unused-lex-cats))
    (when overused-lex-cats
      (warn "The following lexical categories where also defined as non-terminals:~% ~{~a ~}"
	    overused-lex-cats))))

;----------------------------------------------------------------------------;
; expand-Kleene-constituent
;--------------------------
; handle Kleene * and +:  adds to *Kleene+-rules*
;; * case will expand:
;; (defrule <X>*
;;  ::= ()
;;  ::= <x> <X>*-rest)
;; (defrule <X>*-rest
;;  ::= ()
;;  ::= <Sep> <x> <X>*-rest)
;; in case of default seperator " ":
;; (defrule <X>*
;;  ::= ()
;;  ::= <x> <X>*)
;; + case will expand:
;; (defrule <X>+ 
;;  ::= <x> :build (make-kb-sequence :first <x>)
;;  ::= <x> <Sep> <x>+ 
;;      :build (make-kb-sequence :first <x> :rest <x>+))
(defun expand-Kleene-constituent (production-rhs)
  (flet ((new-kb-seq (pairs)
	   (let ((slots (mapcar
			 #'(lambda (pair)
			     (make-LABEL-VALUE-PAIR
			      :-LABEL (first pair) :-VALUE (second pair)))
			 pairs)))
	     (make-feat-term :-type 'kb-sequence
			     :-slots slots)))
	 (memo (item) (push item *Kleene+-rules*)))
    (dolist (constituent (production-rhs--syntax production-rhs))
      (when (Kleene-p constituent)
	(let* ((Kleene-const (Kleene--constituent constituent))
	       (Kleene-Sep (Kleene--Separator constituent))
	       (Kleene+ (encode-separator Kleene-const
					  (Kleene*-p constituent)
					  Kleene-Sep)))
	  (declare (symbol Kleene+))
	  ;; replace the Kleene-expr by a new non-terminal: Kleene+
	  (setf (production-rhs--syntax production-rhs)
		(substitute Kleene+ constituent
			    (production-rhs--syntax production-rhs)))
	  (let ((semantics (production-rhs--semantics production-rhs)))
	    (when (and (feat-term-p semantics)
		       (not (default-separator? Kleene-Sep)))
	      (feat-term-substitute 
	       Kleene+ (decode-kleene-name Kleene+) semantics)))
	  ;; (break "constituent: ~S" constituent) 
	  (unless (find Kleene+ *Kleene+-rules* :key #'zb-rule--name)
	    ;; only if a rule of that name has not been defined yet!
	    (let ((KR-sem (new-kb-seq `((first ,Kleene-const)
					(rest  ,Kleene+)))))
	      (if (Kleene*-p constituent) ; * case
		  (if (default-separator? Kleene-Sep)
		      (memo (make-zb-rule
			     :-name Kleene+
			     :-productions
			     `(,(make-Production-Rhs)
			       ,(make-Production-Rhs
				 :-syntax `(,Kleene-const ,Kleene+)
				 :-semantics KR-sem))))
		    (let ((X*-rest (intern
				    (format nil "Rest-~a"
					    (symbol-name Kleene+)))))
		      (setq KR-sem
			    (new-kb-seq `((first ,Kleene-const)
					  (rest  ,X*-rest))))
		      (memo (make-zb-rule
			     :-name Kleene+
			     :-productions
			     `(,(make-Production-Rhs)
			       ,(make-Production-Rhs
				 :-syntax `(,Kleene-const ,X*-rest)
				 :-semantics KR-sem))))
		      (memo (make-zb-rule
			     :-name X*-rest
			     :-productions
			     `(,(make-Production-Rhs)
			       ,(make-Production-Rhs
				 :-syntax
				 `(,Kleene-Sep ,Kleene-const ,X*-rest)
				 :-semantics KR-sem))))))
		(progn
		  ;; (break "constituent: ~S" constituent) 
		  (memo (make-zb-rule
			 :-name Kleene+
			 :-productions
			 `(,(make-Production-Rhs
			     :-syntax (list Kleene-const)
			     :-semantics (new-kb-seq `((first ,Kleene-const))))
			   ,(make-Production-Rhs
			     :-syntax `(,Kleene-const
					,@(unless (default-separator? Kleene-Sep)
						  (list Kleene-Sep))
					,Kleene+)
			     :-semantics KR-sem)))))))))))
    ;; (format t "~%*Kleene+-rules*: ~{~s ~}" (mapcar #'ZB-RULE--name *Kleene+-rules*))
    ))

(defun default-separator? (Kleene-Sep)
  (member Kleene-Sep '(" " "") :test #'string=))

(defun encode-separator (name k* Sep)
  ;; k* = true iff Kleene operator is *
  ;; k* = false iff Kleene operator is +
  (intern (if (default-separator? Sep)
	      (format nil "~S~:[+~;*~]" name k*)
	    (format nil "~S~:[+~;*~]~A~D$"
		    name
		    k*
		    Sep
		    (length Sep)))))

(defun decode-kleene-name (name)
  (let* ((s (symbol-name name))
	 (s-length (length s))
	 (n (schar s (- s-length 2)))
	 (sep-length (- (char-int n) (char-int #\0))))
    (intern (subseq s 0 (- s-length sep-length 2)))))

(defun constituent-name (constituent)
  ;; constituent:symbol
  ;; strip off .<N> from constituent symbol, unless it ends in $
  (let* ((n (symbol-name constituent))
	 (last-char-pos (1- (length n))))
    (if (char= (schar n last-char-pos) #\$)
	constituent
      (let ((p (position-if #'(lambda (c) (char= c #\.)) n
			    :from-end t)))
	(if (and p 
		 (let ((p+1 (1+ p)))
		   (and (= p+1 last-char-pos)
			(digit-char-p (schar n p+1)))))
	    (intern (subseq n 0 p) (symbol-package constituent))
	  constituent)))))

(defun feat-term-substitute (new old ft)
  (dolist (slot (feat-term--slots ft))
    (let ((val (label-value-pair--value slot)))
      (if (eq val old)
	  (setf (label-value-pair--value slot) new)
	(when (feat-term-p val)
	    (feat-term-substitute new old val))))))

(defun parse-defrule (rule &aux name)
  (unless (and (consp rule) 
	       (symbolp (car rule))
	       (string= (string (car rule)) "DEFRULE")
	       (consp (cdr rule))
	       (symbolp (setq name (cadr rule))))
    (error "Illegal rule ~S" rule))
  (let ((args (cddr rule)) rhs)
    (flet ((parse-build (&key form type map)
	     (cond ((and (not form) type)
		    (if (symbolp type)
			(setf form (generate-form type map))
		      (error "Symbol expected as value of :type ~S in ~S"
			     type rhs))))
	     (multiple-value-bind (ll dummies)
		 (make-lambda-list rhs)
	       (setq dummies
		     (nconc dummies
			    (mapcan #'(lambda (l)
					(unless (member l dummies)
					    (unless (search-list l form)
						(list l))))
				    ll)))
	       ;; now generate the functions from the actions
	       `(lambda ,ll
		 ,@(when dummies `((declare (ignore .,dummies))))
		 ,form)
	       )))
      (let ((R (make-zb-rule :-name name)) action rest)
	(do ((args args rest))
	    ((null args)
	     (setf (zb-rule--productions r) (nreverse (zb-rule--productions r)))
	     R)
	  (let ((key (car args))
		(val (cadr args)))
	    (setq rest (cddr args))
	    (if (eq key ':=)
		(progn
		  (setq rhs (if (listp val) val (list val)))
		  (if (and (consp rest) (eq (car rest) ':BUILD))
		      ;; BUILD clause: construct fn and compile it
		      (let ((build-args (cadr rest)))
			(setq action
			      (if (atom build-args)
				  (if (symbolp build-args)
				      build-args
				    (parse-build :FORM build-args))
				(if (keywordp (car build-args))
				    (apply #'parse-build build-args)
				  (parse-build :FORM build-args))))
			(setq rest (cddr rest)))
		    ;; no :BUILD clause, use IDENTITY fn
		    (setq action 
			  (if (= (length rhs) 1) 'identity 'identity*))))
	      (error "Keyword expected in rule ~S at .. ~{~S ~}~% Probably no () around rule's rhs"
		     name args))
	    (push (make-production-rhs :-syntax rhs
				       :-build-fn action)
		  (zb-rule--productions r))))))))

(defun cons-avm (Feat-Term)
  (let ((type (Feat-Term--type Feat-Term)))
    (cons
     (intern (concatenate 'string "MAKE-"
			  (symbol-name type))
	     (symbol-package type))
     (mapcan
      #'(lambda (lvp)
	  (declare (type Label-value-pair lvp))
	  (let ((slot (Label-value-pair--label lvp))
		(val (Label-value-pair--value lvp)))
	    (list (intern (string slot) *keyword-package*)
		  (if (Feat-Term-p val)
		      (cons-avm val)
		    val))))
      (Feat-Term--slots Feat-Term)))))

(defun process-semantics (production-rhs)
  (let ((Syntax (production-rhs--syntax production-rhs))
	(Feat-Term (production-rhs--semantics production-rhs)))
    (flet ((msg ()
	     (format nil "The Semantics ~S of the rule RHS:~%  ~A~%"
		     Feat-Term
		     (with-output-to-string (s)
		       (print-production-rhs production-rhs s nil)))))
      (flet ((cons-lambda (ft?)
	       (multiple-value-bind (ll dummies)
		   (make-lambda-list Syntax)
		 `(lambda ,ll
		   ,@(when dummies `((declare (ignore .,dummies))))
		   ,(if ft? (cons-avm Feat-Term) Feat-Term)))))
	(setf (production-rhs--build-fn production-rhs)
	      (typecase Feat-Term
		(NULL (if (= 1 (length syntax))
			  'identity
			'identity*))
		((or number string) 
		 `(lambda (&rest args) (declare (ignore args))
		   ,Feat-Term))
		(symbol
		 (if (member Feat-Term Syntax)
		     (cons-lambda nil)
		   (error "~A is a variable that does not occur in the RHS!"
			  (msg))))
		(Feat-Term (cons-lambda t))
		(T (error "~A should be a feature term, number, string or constituent!" (msg)))))))))

	  
;----------------------------------------------------------------------------;
; generate-form
;--------------
; 
; 
(defun generate-form (type map)
  `(,(intern (concatenate 'string "MAKE-" (symbol-name type))
      (symbol-package type))
    ,@(mapcan
       #'(lambda (pair)
	   (unless (consp pair)
	     (error "Element of :map must be a dotted pair in ~S"
		    map))
	   (let ((constituent (car pair))
		 (slot  (cdr pair)))
	     (unless (symbolp constituent)
	       (error "Symbol expected in map ~S at ~S"
		      map constituent))
	     (unless (keywordp slot)
	       (error "Keyword expected in map ~S at ~S"
		      map slot))
	     (list slot constituent)))
       map)))

(defvar *dummy-count* 0)

(defun next-dummy ()
  (let* ((root "DUMMY")
	 (dummy (intern (if (zerop *dummy-count*)
			    root
			  (format nil "DUMMY~S" *dummy-count*)))))
    (incf *dummy-count*)
    dummy))

(defun make-lambda-list (constituents)
  (let ((*dummy-count* 0) dummies)
    (values (mapcar #'(lambda (constituent)
			(if (symbolp constituent)
			    constituent
			  (let ((d (next-dummy)))
			    (push d dummies)
			    d)))
		    constituents)
	    dummies)))

;; search the list for atom and return T if atom occurs anywhere
;; this is overly cautious and should be replaced by a tree-walker
;; but it will only cause some warnings of the compiler.
(defun search-list (atom tree)
  (if (atom tree)
      (eq atom tree)
    (when (consp tree)
	(dolist (n tree)
	   (when (search-list atom n) (return t))))))
      
	
#||
(apply #'parse-build '( "(" Formula ")" ) '(:form (progn Formula)))
(apply #'parse-build '(Identifier) '(:type Propositional-variable
				     :map ((Identifier . :-name))))
(apply #'parse-build '(Formula.1 "and" Formula.2)
       '(:type Boolean-And
	 :map ((Formula.1 . :-rand1)
	       (Formula.2 . :-rand2))))
||#  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Top level load function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD-GRAMMAR loads a Zebu source grammar and prepares it for 
;;; compilation
;;; Internalize a grammar in the lisp syntax described above.
;;; Set up data structures as described above.
;;; Every grammar interns the empty string as a grammar symbol
;;; Generate the hierarchy, if a :domain is specified
;;; and if *generate-domain* is true.

(defun get-grammar-options-key (name)
  (do ((options *grammar-options* (cddr options)))
      ((null options) nil)
    (when (eq (car options) name) (RETURN (cadr options)))))

(defun load-grammar (filename &key (verbose T)
			      &aux (g-file (probe-file filename)))
  (unless g-file
    (error "grammar file not found: ~S" filename))
  (format t "~%Reading grammar from ~A~%" filename)
  (initialize-grammar)
  ;; read first form (possibly twice -- in the right package)
  (let ((grammar-stream (open g-file :direction :input)))
    (unwind-protect
	 (progn
	   (setq *grammar-options*
		 (catch 'read-grammar-options
		   (check-grammar-options
		    (read grammar-stream) g-file t)))
	   (unless *grammar-options*
	     (close grammar-stream)
	     (setq grammar-stream (open g-file :direction :input))
	     (setq *grammar-options*
		   (catch 'read-grammar-options
		     (check-grammar-options
		      (read grammar-stream) g-file t))))
	   (setq *lex-cats* (get-grammar-options-key ':lex-cats))
	   (if (eq *compiler-grammar* *NULL-Grammar*)
	       (let ((eof (list nil)))  
		 (pre-process-rules
		  #'(lambda ()
		      (loop (let ((rule (read grammar-stream nil eof)))
			      (when verbose (print rule))
			      (if (consp rule)
				  (if (eq rule eof)
				      (return nil)
				    (if (eq (car rule) 'defstruct)
					(push rule *domain-structs*)
				      (return (parse-defrule rule))))
				(warn "In file ~a~% illegal rule ~s ignored!"
				       g-file rule)))))
		  nil))
	     (let (*preserve-case*
		   *Kleene+-rules*
		   (ff (file-parser-aux
			grammar-stream #'error t *compiler-grammar*
			verbose)))
	       (pre-process-rules
		#'(lambda ()
		    (loop
		     (let ((f (or (pop ff) (pop *Kleene+-rules*))))
		       (if (null f)
			   (return nil)
			 (if (zb-rule-p f)
			     (return f)
			   (push f *domain-types*))))))
		t))))
      (close grammar-stream)))
  (format t "~%~S productions, ~S symbols~%"
	  *production-count* *g-symbol-count*)
  (setq *symbol-array* (list->vector (reverse *symbols*)))
  (unless *start-symbol* (error "No start symbol"))
  g-file)

;;;------------------------------------------------------------------------;
;; dump-domain-file
;;;------------------------------------------------------------------------;
;; generate code for domain, printers, and regular expressions
;; dump it onto the domain-file
;; it may be the case that none of the above are necessary, in which 
;; case no domain-file is generated
;; the domain-file is specified as:
;;   name: from grammar-option :DOMAIN-FILE
;;   type: the first element of *load-source-pathname-types*
;;   directory: same as grammar-file
;;              if not directory in grammar-file from
;;                      *default-pathname-defaults*
;; if such a file exists already, a warning is given and the old file 
;; is renamed.

(defun dump-domain-file (grammar-file verbose)
  (let* ((domain-file 
	  (merge-pathnames
	   (or (get-grammar-options-key ':DOMAIN-FILE)
	       (make-pathname
		:name (format nil "~A-domain"
			      (get-grammar-options-key ':NAME))))
	   (merge-pathnames
	    (merge-pathnames (make-pathname
			      :type (first *load-source-pathname-types*))
			     grammar-file)
	    *default-pathname-defaults*)))		 
	 (*print-array* t)		; bit-vectors of regex code
	 *print-level* *print-length* *print-circle*
	 written?)
    #-MCL (when (probe-file domain-file)
	    (warn "Renaming existing domain file ~a" domain-file))
    (with-open-file (port domain-file
			  :if-does-not-exist :create
			  :if-exists #-MCL :rename #+MCL :supersede
			  :direction :output)
      (format port ";;; This file was generated by Zebu (Version ~a)~%~%(IN-PACKAGE ~S)~%(REQUIRE \"zebu-package\")~%(USE-PACKAGE \"ZEBU\")~%"
	      zb:*zebu-version* (package-name *package*))

      (when *generate-domain* 
	(format t "~%Generating domain source code onto file: ~a"
		domain-file)
	(setq written? (generate-domain-file domain-file port)))

      ;; Write actions onto domain file
      (when verbose
	(format t "~%Writing actions of rules to ~a" domain-file)
	(terpri port))
      (dolist (r *zb-rules*)
	(let ((non-terminal (car r)))
	  (when verbose (format t "~%Rule ~S" non-terminal))
	  (dolist (production (zb-rule--productions (cdr r)))
	    (let ((fn (production-rhs--build-fn production)))
	      (when (consp fn)
		(let ((fn-name (gentemp (symbol-name non-terminal))))
		  (when verbose (format t " Action: ~S" fn-name))
		  (setf (production-rhs--build-fn production) fn-name)
		  (pprint `(defun ,fn-name . ,(cdr fn)) port)
		  (terpri port)
		  (setq written? t)))))))
      (terpri port)
      ;; for lexical categories: compile the rx-token parsers!
      (when *lex-cats*
	(pprint '(eval-when (compile)
		  (unless (member "zebu-regex" *modules* :test #'equal)
		    (WARN "Load the Zebu Compiler!")))
		port)
	(pprint '(declaim (special *REGEX-GROUPS* *REGEX-GROUPINGS*))
		port)
	(dolist (lex-cat *lex-cats*)
	  (pprint (def-regex-parser (car lex-cat) (cadr lex-cat))
		  port)
	  (terpri port))
	(setq written? t))
      (when written?
	(nconc *grammar-options* (list ':DOMAIN-FILE
				       (namestring domain-file)))
	domain-file))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           End of zebu-loadgram.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
