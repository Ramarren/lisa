; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         zebu-generator.lisp
; Description:  Generate Domain and Print-Functions for the grammar
; Author:       Joachim H. Laubsch
; Created:      25-Feb-92
; Modified:     Wed Jan 13 10:16:30 1999 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/Attic/zebu-generator.lisp,v 1.1 2000/10/12 02:39:44 youngde Exp $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: zebu-generator.lisp,v $
; RCS Revision 1.1  2000/10/12 02:39:44  youngde
; RCS Added Zebu as contrib; initial class files; initial grammar
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(IN-PACKAGE  "ZEBU")

(declaim (special
          *identifier-continue-chars*
          *identifier-start-chars*
          *domain-structs*
          *domain-types*
          *grammar-options*
          *lex-cats*
          ))

;----------------------------------------------------------------------------;
; generate-domain-file
;---------------------
; Generate the DEFSTRUCT calls to define the domain & dump to FILE
; When using the meta-grammar, printers will be compiled too.

; file is open when generate-domain-file is called.
; return true if anything was written.

; If DEFSTRUCT is used in the grammar file  -- *domain-structs*  is not
; () -- the domain does not need to be generated.

(defun generate-domain-file (file port &aux domain printers)
  (unless *domain-structs*
    (when (setq domain
		(prepare-domain
		 (or (get-grammar-options-key ':DOMAIN)
		     ;; set the domain keyword, s.t. at load time
		     ;; the domain definition is present
		     (let ((d (process-domain-definition)))
		       (when d
			 (nconc *grammar-options*
				(list ':DOMAIN d)))
		       d))))		; sets *domain-type-hierarchy*
      (when (string= (grammar-name *compiler-grammar*)
		     "zebu-mg")
	(format t "~%Generating Print-functions ..")
	(setq printers (gen-printers)))))
  (format t "~%Writing domain to ~a~%" file)
  ;; Dump out hierarchy
  (let* ((structs (or (reverse *domain-structs*)
		      (generate-domain domain printers)))
	 (CL-pkg  #-LUCID (find-package "COMMON-LISP")
		  #+LUCID (find-package "LUCID-COMMON-LISP"))
	 (Lisp-pkgs
	  (cons CL-pkg (package-use-list CL-pkg))))
    (dolist (f structs)
      (let ((struct-name (defstruct-name f)))
	(when (member (symbol-package struct-name)
		      Lisp-pkgs)
	  (warn "~s was chosen as the name of domain type, ~%but the symbol is already defined in the ~s"
		struct-name (symbol-package struct-name)))	    
	(pprint f port)
	(terpri port)
	;; build the kb-hierarchy even if defstructs are used
	(when *domain-structs*
	  (format port "(ZB::DEF-KB-DOMAIN-TYPE '~s '~s '~s)~%"
		  struct-name
		  (defstruct-super f)
		  (defstruct-slots f)))
	))
    structs))

(defun defstruct-name (x)
  (let ((n (cadr x)))
    (if (listp n) (car n) n)))

(defun defstruct-super (x)
  (let ((n (cadr x)))
    (when (listp n)
      (let ((include (assoc ':include (cdr n))))
	(when include (second include))))))

(defun defstruct-slots (x)
  (mapcar #'(lambda (sd) (if (listp sd) (car sd) sd))
	  (cddr x)))

;----------------------------------------------------------------------------;
; generate-domain
;----------------
; Given domain D and an alist PRINTERS with pairs (<type> . <print-function>)
; return a list of DEFSTRUCT calls

(defun generate-domain (d printers &aux code)
  (flet ((parse-slots (l)
	   (mapcar #'(lambda (s)
		       (if (atom s)
			   s
			 `(,(car s) nil :type (or null ,(cadr s)))))
		   l)))
    (flet ((slots (x)
	     (do ((xrest x (cddr xrest)))
		 ((null xrest) nil)
	       (if (eq (car xrest) ':slots)
		   (return (parse-slots (cadr xrest))))))
	   (make-struct (name include slots constructor?)
	     `(defstruct (,name
			  (:include ,include)
			  ,@(let ((fn (assoc name printers)))
			      (when fn
				`((:print-function ,(cdr fn)))))
			  ,@(unless constructor?
			      (list '(:constructor nil)))
			  )
	       ,@slots)))
      (labels ((generate-domain-aux (sub super args constructor?)
		 (unless (eq sub super)
		   (push (make-struct sub super (slots args) constructor?)
			 code))
		 (do ((xrest args (cddr xrest))) ((null xrest))
		   (when (eq (car xrest) ':subtype)
		     (let ((newsub (cadr xrest)))
		       (if (atom newsub)
			   (push (make-struct newsub sub nil t) code)
			 (generate-domain-aux
			  (car newsub) sub (cdr newsub) t)))))))
	(when d
	  (generate-domain-aux (car d) 'kb-domain (rest d) nil)
	  (nreverse code))))))


;----------------------------------------------------------------------------;
; process-domain-definition
;--------------------------
; Transform the list of DOMAIN-TYPEs into the hierarchical structure
; with root KB-DOMAIN, and :SUBTYPE, :SLOTS arcs
(defun process-domain-definition (&aux (R (list 'KB-domain)))
  (labels ((find-super (node supertype)
	     ;; node is the list form of the domain def
	     (if (null node)
		 'Nil
	       (if (eq (car node) supertype)
		   node
		 (do ((n (cdr node) (cddr n)))
		     ((null n) nil)
		   (when (eq (car n) ':subtype)
		     (let ((r (find-super (cadr n) supertype)))
		       (when r (return r)))))))))
    (when (null *domain-types*)
      (return-from process-domain-definition nil))
    ;; if there is a supertype in *domain-types* that is 
    ;; undefined, define it as a subtype of KB-domain
    (dolist (node *domain-types*)
      (let ((supertype (domain-type--supertype node)))
	(unless (or (eq supertype 'KB-domain)
		    (find supertype *domain-types*
			  :key #'domain-type--type))
	  (push (make-domain-type
		 :-supertype 'KB-domain
		 :-type supertype)
		*domain-types*))))
    ;; transform the sorted list to the external :DOMAIN notation
    (let ((domain-types (copy-list *domain-types*)))
      (loop (or domain-types (return R))
	    (do ((nodes domain-types (cdr nodes)))
		((null nodes))
	      (let* ((node (first nodes))
		     (supertype (domain-type--supertype node))
		     (type (domain-type--type node))
		     (slots (domain-type--slots node))
		     (super (find-super R supertype)))
		(when super
		  (nconc super `(:subtype
				 (,type
				  ,@(if slots `(:slots ,slots)))))
		  (setq domain-types (delete node domain-types)))))))
    ;; (pprint R)
    R))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Generate the print-functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------------------------------------------------------------------;
; Non-terminal-p
;---------------
; 
(defun Non-terminal-p (constituent)
  (and (symbolp constituent) (not (assoc constituent *lex-cats*))))

;----------------------------------------------------------------------------;
; 1-constituent-production-p
;---------------------------
; 
; 
(defun 1-constituent-production-p (syntax)
  (= 1 (count-if #'Non-terminal-p syntax))
  )

(defun first-nt-constituent (syntax)
  (find-if #'Non-terminal-p syntax)
  )

;; collect for each type the lhs and production-rhs in the alist
;; type-prod-AL ( (<type> . %1=( %2=(<lhs> . <production-rhs>) .. )) ..)
;; type-print-fn-AL ( (<type> . (lambda ..)) ..)
;; Return type-print-fn-AL

(defun gen-printers (&aux type-prod-AL type-print-fn-AL user-def-types
			  KB-sequence-print-fn-AL KB-sequence-prods
			  (print-fn-argl (mapcar #'intern 
						 '("ITEM" "STREAM" "LEVEL"))))
  (flet ((memo (type val)
	   (let ((bdg  (assoc type type-prod-AL)))
	     (if (null bdg)
		 (push (cons type (list val)) type-prod-AL)
	       (push val (cdr bdg))))))	   
    (maphash #'(lambda (key val) (declare (ignore val))
		       (unless (or (member key '(:TOP KB-DOMAIN KB-SEQUENCE))
				   (member (symbol-name key)
					   *open-categories*
					   :test #'string=)
				   (assoc key *lex-cats*))
			 (push key user-def-types)))
	     *domain-ht*)
    ;; for each type, gather a set of productions that produce it
    ;; also check that the type and its slots are defined
    (dolist (zb-rule *zb-rules*)
      (let ((lhs (car zb-rule)))
	(dolist (prod (zb-rule--productions (cdr zb-rule)))
	  (let ((semantics (production-rhs--semantics prod)))
	    (if (null semantics)
		(let ((syntax (production-rhs--syntax prod)))
		  (when (1-constituent-production-p syntax)
		    (let ((nt (first-nt-constituent syntax)))
		      (when (eq (car (infer-type-disj-of-expr nt))
				'kb-sequence)
			(memo 'kb-sequence (cons lhs nt))))))		  
	      (when (and semantics (feat-term-p semantics))
		(let* ((type (feat-term--type semantics))
		       (type-node (gethash type *domain-HT*))
		       (slots (feat-term--slots semantics)))
		  ;; warn about inconsistent use of the types	
		  (if (null type-node)
		      (warn "Type: ~S is not defined in this domain" type)
		    (dolist (slot slots)
		      (let ((slot-label (label-value-pair--label slot))
			    (slot-value (label-value-pair--value slot)))
			(if (KB-legal-slot-p type slot-label)
			    (let ((slot-type (KB-slot-type
					      type slot-label)))
			      (unless (eq slot-type ':TOP)
				(unless (every
					 #'(lambda (sub)
					     (is-subtype-of sub slot-type))
					 (infer-type-disj-of-expr slot-value))
				  (warn "~S type restriction of ~S violated by ~S"
					slot-type slot-label slot-value)
				  ;;(break "~%> ~S" prod)
				  )))
			  (warn "Slot: ~S is not defined for ~S"
				slot-label type)))))
		  (memo type (cons lhs prod))
		  (for-each-supertype
		   #'(lambda (node)
		       (setq user-def-types
			     (delete (type-tree-node--label node)
				     user-def-types)))
		   type)))))))))
  (when user-def-types
    (warn "Types:~{ ~S~}~% were defined but not used." user-def-types))

  ;; generate print-function for nonterminals which produce a kb-sequence
  (when (setf KB-sequence-prods (assoc 'KB-sequence type-prod-AL))
    (setf type-prod-AL (delete KB-sequence-prods type-prod-AL)
	  KB-sequence-print-fn-AL
	  (gen-KB-sequence-printers (cdr KB-sequence-prods))))
  ;; (break "KB-sequence-print-fn-AL ~%~S:" KB-sequence-print-fn-AL)
  ;; now generate the print-function for each type
  ;; unless one has been predefined (via the << foo >> Syntax)
  (dolist (e type-prod-AL type-print-fn-AL)
    (let* ((type (car e))
	   (domain-type (find type *domain-types*
			      :key #'domain-type--type))
	   (fun (when domain-type
		  (domain-type-print-function domain-type))))
      ;; (break "domain-type: ~s" domain-type)
      (when (and domain-type (not fun))
	(let ((%1   (cdr e))		; ((<lhs> . <prod-rhs>)..)
	      clauses good-bdgs unused-bdgs)
	  (dolist (%2 %1)		; (<lhs> . <prod-rhs>)
	    (push (gen-print-case %2) clauses))
	  ;; <clause> = (short-lambda-list syntax binding-list)
	  (multiple-value-bind (cond-clauses bindings)
	      (gen-clauses clauses KB-sequence-print-fn-AL)
	    ;; split bindings in good ones and unused ones
	    (dolist (b bindings)
	      (if (null (cdr b))
		  (pushnew b unused-bdgs)
		(pushnew b good-bdgs)))
	    (setf fun `(lambda (,@print-fn-argl 
				,@(when good-bdgs `(&aux .,good-bdgs)))
			  (declare (ignore 
				  ,@unused-bdgs
				  .,(if (not good-bdgs)
					print-fn-argl
				      (cddr print-fn-argl))))
			,(if (cdr cond-clauses)
			     (progn
			       ;; last cond-clause has antecedent T
			       (setf (caar (last cond-clauses)) t)
			       `(cond ,@(simplify-cond-clauses cond-clauses)))
			   ;; the condition must be true
			   (cadar cond-clauses)))))))
      (push (cons type fun) type-print-fn-AL))))

;;------------------------------------------------------------------------;;
;; gen-KB-sequence-printers
;;-------------------------
;; generate in-line format forms for KB-sequence first:
;; KB-sequence-print-fn-AL: ((Constituent . <form(lhs)>)..); 

(defun gen-KB-sequence-printers (prods &aux Alist separator)
  (dolist (prod prods Alist)
    ;; prod = (<lhs> . <prod-rhs>)  |  (<lhs> . <symbol>)
    (let ((lhs (car prod)) (rhs (cdr prod)))
      ;; (format t "~%~%Prod: ~s ::= ~s" lhs (if (symbolp rhs) rhs (production-rhs--syntax rhs)))
      (setq separator (decode-separator (if (symbolp rhs) rhs lhs))
	    Alist (add-print-fn
		   lhs
		   (if separator
		       `(let ((*kb-sequence-separator* ,separator))
			 (declare (special *kb-sequence-separator*))
			 (KB-SEQUENCE-print ,lhs nil nil))
		     `(KB-SEQUENCE-print ,lhs nil nil))
		   Alist))
      ;; (format t "~%Separator: ~s" separator)
      )))

(defun decode-separator (name)
  ;; return NIL for the default separator
  (let* ((s (symbol-name name))
	 (s-length (length s))
	 (last-char-pos (1- s-length)))
    (when (char= (schar s last-char-pos) #\$)
      (let ((sep-ln-char (schar s (1- last-char-pos))))
	(when (digit-char-p sep-ln-char)
	  (let ((sep-length (- (char-int sep-ln-char) (char-int #\0))))
	    (subseq s 
		    (- s-length sep-length 2)
		    (- last-char-pos 1))))))))

;----------------------------------------------------------------------------;
; add-print-fn
;-------------
; add the print-function FN for the non-terminal CONSTITUENT to ALIST
; 
(defun add-print-fn (CONSTITUENT FN ALIST)
  (let ((bdg (assoc CONSTITUENT ALIST)))
    (if (null bdg)
	(acons CONSTITUENT FN ALIST)
      (progn (setf (cdr bdg)
		   `(if (null ,CONSTITUENT)
		     ""
		     ,(if (equal FN "")
			  (cdr bdg)
			FN)))
	     ALIST))))

;----------------------------------------------------------------------------;
; clause
;-------
; <lambda-list syntax binding-list semantics>
; 
(defstruct (clause)
  ll syntax bl semantics
  )

;----------------------------------------------------------------------------;
; gen-print-case
;---------------
; given: (<lhs> . <prod-rhs>)
; return: lambda-list of constituents in lhs
;         syntax of rhs
;         for each var in the lambda-list a path of accessors

(defun gen-print-case (lhs-rhs-pair)
  (let* ((prod (cdr lhs-rhs-pair))
	 (syntax (production-rhs--syntax prod))
	 (semantics (production-rhs--semantics prod))
	 (ll (mapcan #'(lambda (constituent)
			 (unless (stringp constituent) (list constituent)))
		     syntax))
	 (binding-list
	  (mapcar
	   #'(lambda (var)
	       (let ((p (find-path var semantics)))
		 (if (null p)
		     (progn
		       (warn "~:[Lexical Category~; Non-Terminal~] ~S not used in semantics ~% of ~S."
			     (Non-terminal-p var) var (car lhs-rhs-pair))
		       (list var)
		       )
		   (cons var p))))
		  ll)))
    (make-clause
     :ll ll :syntax syntax :bl binding-list :semantics semantics)))

;----------------------------------------------------------------------------;
; gen-clauses
;------------
; Given clauses of the form:
;  <clause> = <short-lambda-list syntax binding-list semantics>
; where binding-list = ((<non-terminal-symbol> . <path to access from ITEM>) ..)
; return: (1) ((<test for print-case> <format stmt derived from syntax>) ..)
;         (2) a lambda-list binding the %u .. variables used to accessors
;             derived from the paths.
(defconstant *vars-to-use* '("%R" "%S" "%T" "%U" "%V" "%W" "%X" "%Y" "%Z"))

(defun gen-clauses (clauses KB-sequence-print-fn-AL
			    &aux (vars-to-use (mapcar #'intern *vars-to-use*))
			    ;; a set of sets with the same print syntax
			    (partitioning (partition-set #'same-print-syntax clauses))
			    alist cond-clauses)
  (labels ((memo-path (path)
	     (let ((bdg (assoc path alist :test #'equal)))
	       (if bdg
		   (cdr bdg)
		 (let ((R (pop vars-to-use)))
		   (push (cons path R) alist)
		   R))))
	   (make-format (syntax bdgs)
	     (when syntax
	       (let ((R `(format ,(intern "STREAM")
			  ,(apply #'concatenate 'string
				  (make-format-string-list syntax))
			  ,@(mapcan
			     #'(lambda (const) 
				 (unless (stringp const) 
				   (let ((seq-fn-bdg
					  (assoc
					   const
					   KB-sequence-print-fn-AL))
					 (var (let ((bdg (cdr (assoc const bdgs))))
						(when bdg (memo-path bdg)))))
				     (list
				      (if seq-fn-bdg
					  `(let ((,const ,var))
					    ,(cdr seq-fn-bdg))
					(or var
					    (warn "Can't unparse ~s~%~s is unbound in semantics"
						  syntax const)))))))
			     syntax))))
		 ;; (format t "~%format: ~s ~a -->~% " syntax bdgs) (pprint R) (break "gen-clauses")
		 R))))
    (dolist (eq-print-set partitioning)
      (let (ante (proto  (first eq-print-set)))
	(dolist (eq-print eq-print-set)
	  (let ((ll     (clause-ll eq-print))
		(bdgs   (clause-bl eq-print)))
	    (pushnew
	     (if (null ll)
		 (progn 
		   ;; (break  "sem: ~s" (clause-semantics eq-print))
		   `(equal ,(intern "ITEM") ,(cons-avm
					      (clause-semantics eq-print))))
	       (let* (type-list		; type-preds that have to hold
		      (ll-map		; ((<lvar> . <%var>) ..)
		       (mapcar #'(lambda (var)
				   (push (infer-type-predicate var)
					 type-list)
				   (cons var
					 (memo-path
					  (cdr (assoc var bdgs)))))
			       ll))
		      (conjuncts
		       (mapcan #'(lambda (lvar type)
				   (if (consp type)
				       `((typep ,(cdr lvar) ',type))
				     (if (eq type 'T)
					 ;; delete the variables for which
					 ;; we could not infer a type
					 ()
				       `((,type ,(cdr lvar))))))
			       ll-map (nreverse type-list))))
		 (if (cdr conjuncts)
		     `(AND . ,conjuncts)
		   (car conjuncts))))
	     ante :test #'equal)))
	(setq ante (if (cdr ante) (cons 'OR ante) (car ante)))
	(setq cond-clauses
	      (insert-clause `(,ante
			       ,(make-format (clause-syntax proto)
					     (clause-bl proto)))
			     cond-clauses))))
    (values cond-clauses
	    (mapcar #'(lambda (pair)	; (<path> .  <%var>)
			(list (cdr pair)
			      (path-to-form (car pair) (intern "ITEM"))))
		    alist))))

(defun path-to-form (path target)
  (reduce #'(lambda (a b) (list b a))
	  path
	  :initial-value target))
;----------------------------------------------------------------------------;
; insert-clause
;--------------
; insert stronger clause at front
; 
(defun insert-clause (clause clauses)
  (flet ((conjunction? (x) (and (consp x) (eq (car x) 'AND)))
	 (conjuncts (x) (rest x))
	 (typed-var? (x) (and (consp x) (eq (car x) 'TYPEP)))
	 (typed-var-nm (x) (cadr x))
	 (typed-var-type (x) (cadr (caddr x)))
	 (ante (x) (car x)))
    (flet ((weaker-typed? (ante1 ante2)
	     (and (typed-var? ante1)
		  (typed-var? ante2)
		  (eq (typed-var-nm ante1) (typed-var-nm ante2))
		  (kb-subtypep (typed-var-type ante2)
			       (typed-var-type ante1)))))
      (if (null clauses)
	  (list clause)
        (let ((ante1 (ante clause)))
	  ;; (format t "~%a1: ~S~%a2s: ~{~A~%~}" (ante clause) (mapcar #'ante clauses))
	  (if (member ante1 clauses :test #'equal :key #'ante)
	      ;; the antecedent is already in the clauses
	      ;; this indicate a many-to-one surface-to-abstract syntax
	      clauses
	    (let* ((clause2 (first clauses)) (ante2 (ante clause2)))
	      (if (conjunction? ante2)
		  (if (conjunction? ante1)
		      (if (subsetp ante1 ante2 :test #'weaker-typed?)
			  (cons clause2 (insert-clause clause (rest clauses)))
			(cons clause clauses))
		    (if (typed-var? ante1)
			(if (find-if #'(lambda (a) (weaker-typed? ante1 a))
				     (conjuncts ante2))
			    (cons clause2 (insert-clause clause (rest clauses)))
			  (cons clause clauses))
		      (cons clause2 (insert-clause clause (rest clauses)))))
	        (if (conjunction? ante1)
		    ;; ((and p q) ..) : p  --> ((and p q) p ..) 
		    (if (typed-var? ante2)
			(if (find-if #'(lambda (a) (weaker-typed? a ante2))
				     (conjuncts ante1))
			    (cons clause clauses)
			  (cons clause2 (insert-clause clause (rest clauses))))
		      ;; ante2 is not typed, eg. (IDENTIFIERP %U)
		      (if (member ante2 (conjuncts ante1) :test #'equal)
			  (cons clause clauses)
			(cons clause2 (insert-clause clause (rest clauses)))))
		  ;; both are simple
		  (if (weaker-typed? ante1 ante2)
		      (cons clause2 (insert-clause clause (rest clauses)))
		    (cons clause clauses)))))))))))

;----------------------------------------------------------------------------;
; same-print-syntax
;------------------
; Given clauses rhs A and B
; where  <clause> = short-lambda-list syntax binding-list
; return true iff
; the syntax's constants are the same and its variables have the same bdg
(defun same-print-syntax (a b)
  (let ((a-syntax (clause-syntax a)) (b-syntax (clause-syntax b)))
    (and (equal (length a-syntax) (length b-syntax))
	 (every #'(lambda (constituent1 constituent2)
		    (or (and (symbolp constituent1) (symbolp constituent2))
			(and (stringp constituent1)
			     (stringp constituent2)
			     (string= constituent1 constituent2))))
		a-syntax b-syntax)			
	 ;; syntax is the same
	 (let ((a-bdgs (clause-bl a)) (b-bdgs (clause-bl b)))
	   ;; do all variables of the lambda-list have the same path?
	   (every #'(lambda (u v)
		      (equal (cdr (assoc u a-bdgs))
			     (cdr (assoc v b-bdgs))))
		  (clause-ll a)
		  (clause-ll b))))))

;----------------------------------------------------------------------------;
; make-format-string-list
;------------------------
; This converts a rhs of a grammar rule (SYNTAX) to a format string. 
; It tries to infer when spaces should be inserted based on the
; parameter *identifier-continue-chars*
; As a "rule of style" if a token has a space to its left (right) it should
; also have one to its right (left), unless the token is the last in syntax.
(defun make-format-string-list (syntax)
  (let ((sep-sq (insert-seperator? syntax))
	(a-tok "~a")
	(s-tok "~s")
	(blank " ")
	pre-sep?)
    ;; const1 const2 ... constn
    ;; sep1   sep1   ... sep1
    ;; enforce the rule of style that a grammar keyword has
    ;; blanks on both sides if it has one on either
    ;; this algorithm is too cautious, since it does not hurt to
    ;; introduce a blank!
    (do ((syn-tl syntax (cdr syn-tl))
	 (sep-tl sep-sq (cdr sep-tl))
	 Acc				; accumulated result
	 )
	((null syn-tl) (nreverse Acc))
      (let ((const (car syn-tl))
	    (sep?  (car sep-tl))
	    (preceding-blank? (and Acc (eql (first Acc) blank))))
	(if (stringp const)
	    (progn
	      ;;(break "constituent= ~s" const)
	      (when (and pre-sep?
			 (not preceding-blank?)
			 (insert-seperator-before? const))
		(setq preceding-blank? t) (push blank Acc))
	      (push (escape-tilde const) Acc)
	      (when (or sep?
			;; there is a preceding blank, and not at end
			(and (cdr syn-tl) 
			     preceding-blank?
			     (parse-id/number? (second syn-tl))))
		(push blank Acc)))
	  (let ((firsts (first-terminal (constituent-name const))))
	    (if (and (null (rest firsts))
		     (string= "STRING" (first firsts)))
		(push s-tok Acc)
	      (push a-tok Acc))
	    (when (or sep?
		      ;; there is a preceding blank, and not at end
		      (and (cdr syn-tl) 
			   preceding-blank?
			   (parse-id/number? (second syn-tl))))
	      (push blank Acc))))
	(setq pre-sep? sep?)
	;; (format t "~%Acc: |~{~a~}|" (reverse Acc))
	))))

(defun escape-tilde (string)
  ;; precede each ~ by ~
  (declare (string string))
  (let* ((R "")
	 (tilde #\~)
	 (p0 0)
	 (p1 (position tilde string :test #'eql)))
    (declare (fixnum p0))
    (if p1
	(loop (setq R (concatenate
		       'string R (subseq string p0 p1) "~~"))
	      (setq p0 (1+ p1))
	      (unless (setq p1 (position tilde string
					 :start p0 :test #'eql))
		(return-from escape-tilde
		  (concatenate 'string R (subseq string p0)))))
      string)))

(defun parse-id/number? (const)
  (when (stringp const)
    (let* ((s const) (n (length s)) state)
      (declare (string s))
      (or				; number
       (dotimes (i n t)
	 (let ((c (schar s i)))
	   (if (null state)
	       (if (digit-char-p c)
		   nil
		 (if (eql c #\.)
		     (setq state t)
		   (return nil)))
	     (if (digit-char-p c)
		 nil
	       (return nil)))))
					; id
       (setq state nil)
       (dotimes (i n t)
	 (let ((c (schar s i)))
	   (if (null state)
	       (if (find c *identifier-start-chars*)
		   (setq state t)
		 (return nil))
	     (if (find c *identifier-continue-chars*)
		 nil
	       (return nil)))))))))

(defun continues-token? (e)
  (declare (string e))
  (or (zerop (length e))
      (let ((c (schar e 0)))
	(declare (character c))
	(if (find c *identifier-continue-chars*)
	    t
	  (or (digit-char-p c)
	      (eql c #\.))))))

(defun insert-seperator? (s)
  ;; -> seq of T/Nil depending on whether the element in s should
  ;; be followed by a seperator
  (declare (list s))
  (maplist #'(lambda (s-tl)
	       (let ((e1 (first s-tl)))
		 (if (null (rest s-tl))
		     ;; e1 is the last element
		     ;; by default no seperator after the last const
		     nil
		   ;; compare e1 to next element, e2
		   (let ((e2 (second s-tl)))
		     (if (symbolp e1)
			 (if (symbolp e2)
			     t
			   ;; the following string e2 could continue
			   ;; the id e1
			   (continues-token? (the string e2)))
		       (let ((ln1 (length e1)))
			 (if (= 0 ln1)
			     nil
			   (if (symbolp e2)
			       ;; e1 is a string
			       ;; follow it by space if
			       ;; it ends neither in white-space
			       ;; nor in a char not in *identifier-continue-chars*
			       ;; nor a digit
			       (insert-seperator-after? e1)
			     ;; both e1 and e2 are strings
			     ;; could they parse as a number or an id?
			     (and (parse-id/number? e1)
				  (continues-token? (the string e2)))))))))))
	   s))

(defun white-space-p (char)
  (let ((w (or (get-grammar-options-key ':white-space)
	       '(#\Space #\Newline #\Tab))))  
    (member (the character char) w :test #'char=)))

(defun insert-seperator-before? (const)
  (or (symbolp const)
      (let ((ln (length const)))
	(or (zerop ln)
	    (let ((c0 (schar const 0)))
	      (if (find c0 *identifier-continue-chars*)
		  T
		(or (digit-char-p c0)
		    (white-space-p c0))))))))

(defun insert-seperator-after? (const)
  (or (symbolp const)
      (let ((ln (length const)))
	(or (zerop ln)
	    (let ((last-char (schar const (1- ln))))
	      (if (white-space-p last-char)
		  nil
		(if (find last-char *identifier-continue-chars*)
		    T
		  (digit-char-p last-char))))))))

;----------------------------------------------------------------------------;
; simplify-cond-clauses
;----------------------
;  ((and a1 b1) c1)
;  ((and a1 b2) c2 ..) ..
; (cond (a1 (cond (b1 c1) (b2 c2))) ..

(defun simplify-cond-clauses (clauses)
  (flet ((conj1 (cl) (second cl))
	 (conj2 (cl) (third cl))
	 (and? (cl) (and (consp cl) (eq (car cl) 'AND))))
    (let* ((cl1 (first clauses))
	   (ante1 (car cl1))
	   (rest1 (cdr cl1)))
      (if (and (and? ante1) (rest clauses))
	  (let* ((cl2 (second clauses))
		 (ante2 (car cl2))
		 (rest2 (cdr cl2)))
	    (if (and (and? ante2) (equal (conj1 ante1) (conj1 ante2)))
		`((,(conj1 ante1) (cond (,(conj2 ante1) .,rest1)
					(,(conj2 ante2) .,rest2)))
		  .,(cddr clauses))
	      clauses))
	clauses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Type Inference for Non-Terminals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type->predicate (type)
  (case type
    (NUMBER     'numberp)
    (IDENTIFIER 'identifierp)
    (STRING     'stringp)
    (t (intern (concatenate 'string (symbol-name type) "-P")
	       (symbol-package type)))))

(defun infer-type-predicate (var &aux (v (constituent-name var)))
  (case v
    (NUMBER     'numberp)
    (IDENTIFIER 'identifierp)
    (STRING     'stringp)
    (t (let ((type (infer-type v)))
	 (if (eq type ':TOP)
	     (let ((domain-top (car (type-tree-node--subtypes
				     *domain-type-hierarchy*))))
	       `(OR
		 ,(type-tree-node--label domain-top)
		 NUMBER SYMBOL STRING))
	   (if (consp type)
	       (if (null (rest type))
		   (type->predicate (first type))
		 (cons 'OR type))
	     (if (null type)
		 'T
	       (type->predicate type))))))))


(defun infer-type (v)
  (if (member v '(NUMBER IDENTIFIER STRING))
      v
    (let ((disj (infer-type-disj v)))
      (if (null disj)
	  (warn "Could not infer type for ~S" v)
	disj))))

;----------------------------------------------------------------------------;
; is-subtype-of
;--------------
; 
; 
(defun is-subtype-of (a b)
  (or (eq a b)
      (let ((type-nd (gethash a *domain-HT*)))
	(when type-nd
	  (let ((sup (type-tree-node--supertype type-nd)))
	    (and sup
		 (is-subtype-of (type-tree-node--label sup) b)))))))

;----------------------------------------------------------------------------;
; kb-subtypep
;------------
; disjunctive and conjunctive types are allowed
; 
(defun kb-subtypep (a b)
  (if (consp a)
      (case (first a)
	(OR (every #'(lambda (junct) (kb-subtypep junct b))
		   (rest a)))
	(AND (some #'(lambda (junct) (kb-subtypep junct b))
		   (rest a)))
	(T nil))
      (if (consp b)
	  (case (first b)
	    (OR (some #'(lambda (junct) (kb-subtypep junct a))
		      (rest b)))
	    (AND (every #'(lambda (junct) (kb-subtypep junct a))
			(rest b)))
	    (T nil))
	(is-subtype-of a b))))

(defun check-domain-type (type node)
  (unless type
    (error "~S is not a defined domain type." node)))

(defun infer-type-disj (v &aux (nts (list v)))
  ;; return a list of the possible types for a non-terminal V
  (labels ((infer-type-aux (v disjuncts)
	     (if (or (member v '(NUMBER IDENTIFIER STRING))
		     (assoc v *lex-cats*))
		 (adjoin v disjuncts) 
	       (let ((zb-rule (assoc v *zb-rules*))
		     (types disjuncts))
		 (unless zb-rule
		   (error "No Rule/Non-terminal named ~s found" v))
		 (dolist (prod (zb-rule--productions (cdr zb-rule)) types)
		   (let ((s (production-rhs--semantics prod)))
		     (if s
			 (if (feat-term-p s)
			     (setq types (adjoin-type-disj
					  (feat-term--type s) types))
			   (dolist (type (infer-type s))
			     (setq types (adjoin-type-disj type types))))
		       (let ((nt (find-if #'symbolp
					  (production-rhs--syntax prod))))
			 (unless (or (null nt) (member nt nts))
			   (push nt nts)
			   (setq types
				 (infer-type-aux nt types)))))))))))
    (infer-type-aux v nil)))

(defun adjoin-type-disj (type disj)
  (if (find type disj :test #'is-subtype-of)
      disj
    (cons type (delete-if #'(lambda (a) (is-subtype-of a type))
			  disj))))
					   
  
(defun infer-type-disj-of-expr (x)
  (typecase x
    (number '(number))
    (string '(string))
    (symbol (infer-type-disj (constituent-name x)))))

;----------------------------------------------------------------------------;
; find-path
;----------
; Given a typed feature-structure feat-term, and a variable V occuring
; somewhere as a value of a slot, return a path to it
; return: (1) if you are there ()
;         (2) if there is no path to v: :FAIL
;         (3) if there is some path: the first one found

(defun find-path (v feat-term)
  (labels ((find-path-aux (avl)
	     (if (atom avl)
		 (if (feat-term-p avl)
		     (find-path-list (feat-term--slots avl)
				     (feat-term--type avl))
		   (if (eq v avl)
		       t
		     :FAIL))
	       :FAIL))
	   (find-path-list (avl type)
	     (dolist (lv-pair avl)
	       (let ((p (find-path-aux (label-value-pair--value lv-pair))))
		 (unless (eq p :FAIL)
		   (return
		     (cons (intern
			    (concatenate
			     'string
			     (symbol-name type) "-"
			     (symbol-name (label-value-pair--label lv-pair)))
			    (symbol-package type))
			   (if (eq p 't) nil p))))))))
    (find-path-aux feat-term)))

;----------------------------------------------------------------------------;
; partition-set
;--------------
; partition SET according to EQUIV-FN
; for equiv-fn holds (equiv-fn x y) = (equiv-fn y x)

(defun partition-set (equiv-fn set &aux alist)
  (do ((x-set set (cdr x-set))) ((null x-set))
    (let ((x (car x-set)))
      (push (list x) alist)
      (do ((y-set (cdr x-set) (cdr y-set))) ((null y-set))
	(let ((y (car y-set)))
	  (if (funcall equiv-fn x y)
	      (let ((found-association (assoc x alist)))
		(push y (cdr found-association))))))))
  (labels ((partition-set-aux (alist)
	     (if (null alist)
		 '()
	       (let* ((pair1 (car alist))
		      (set1 (reduce #'union
				    (mapcar
				     #'(lambda (p)
					 (let ((found (find-if
						       #'(lambda (p1)
							   (member p1 p))
						       pair1)))
					   (when found
					     (setf alist (delete p alist))
					     p)))
				     (cdr alist)) 
				    :initial-value pair1)))
		 (cons set1
		       (partition-set-aux (cdr alist)))))))
    (partition-set-aux alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||

(GEN-PRINTERS (find-grammar "ex1a"))
(infer-type 'cl-user::ee)
(infer-type 'cl-user::f)
(infer-type 'cl-user::tt)
(infer-type-predicate 'IDENTIFIER)
(infer-type-predicate 'cl-user::ee)

(PARTITION-SET #'(lambda (x y)
		   (eql (schar (string x) 0)
			(schar (string y) 0)))
	       '(a aa aaa b bbb bb c cccc))

(PARTITION-SET #'(lambda (x y)
			    (eql (schar (string x) 0)
				 (schar (string y) 0)))
	       '(a b c))

;----------------------------------------------------------------------------;
; partition-set-by-selection-fn
;------------------------------
;;; partition set according to selection-fn

(defun partition-set-by-selection-fn (selection-fn set &aux alist)
  (dolist (item set)
    (let* ((key (funcall selection-fn item))
	   (found-association (assoc key alist :test #'eql)))
      (if found-association 
	  (nconc (cdr found-association) (list item))
	(push (cons key (list item)) alist))))
  (dolist (pair alist)
    (setf (car pair) (cadr pair)
	  (cdr pair) (cddr pair)))
  alist)


(partition-set-by-selection-fn #'evenp '(1 2 3 4 5 6 7 8))

 ==> ((2 4 6 8) (1 3 5 7))
||#

#||
;----------------------------------------------------------------------------;
; follow-terminal
;----------------
; given the name of a grammar-symbol, return the
; list of possibly following strings
(defun follow-terminal (name)
  (mapcar #'g-symbol-name
	  (oset-item-list (g-symbol-follow-set (g-symbol-intern name))))
  )
||#

(defun first-terminal (name)
  (mapcan #'(lambda (item)
	      (unless (eq item *empty-string-g-symbol*)
		(list (g-symbol-name item))))
	  (oset-item-list (g-symbol-first-set (g-symbol-intern name)))))

#||
(follow-terminal 'user::ARG)
(first-terminal 'user::ARG*448)
(follow-terminal 'user::ARG*)
(first-terminal 'user::Name)
(first-terminal 'Identifier)

(intersection (follow-terminal 'user::ARG) (first-terminal 'user::ARG*438))
(intersection (follow-terminal 'user::stmt) (first-terminal 'user::stmt+))

||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        End of zebu-generator.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
