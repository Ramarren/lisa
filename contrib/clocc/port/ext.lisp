;;; Basic extensions: conditions, compositions &c
;;;
;;; Copyright (C) 1999-2001 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: ext.lisp,v 1.4 2001/06/26 18:08:30 youngde Exp $
;;; $Source: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/clocc/port/Attic/ext.lisp,v $

(defpackage "PORT"
  (:use "COMMON-LISP")
  (:nicknames "ORG.CONS.CLOCC/SDS/PORT")
  (:export
   "CODE" "CASE-ERROR" "NOT-IMPLEMENTED" ; conditions
   "DEFSUBST" "DEFCUSTOM" "DEFCONST"
   "MK-ARR" "MAP-IN" "WITH-GENSYMS"
   "GC" "QUIT"
   "+EOF+" "EOF-P" "STRING-TOKENS"
   #-cmu "REQUIRED-ARGUMENT"
   "COMPOSE" "COMPOSE-F" "COMPOSE-ALL"))

(in-package :port)

(setf (logical-pathname-translations "port")
      `(("**;*" ,(logical-pathname "clocc:src;port;**;*"))))

;;;
;;; Conditions
;;;

(define-condition code (error)
  ((proc :reader code-proc :initarg :proc)
   (mesg :type simple-string :reader code-mesg :initarg :mesg)
   (args :type list :reader code-args :initarg :args))
  (:documentation "An error in the user code.")
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~s]~@[ ~?~]" (code-proc cc)
                     (and (slot-boundp cc 'mesg) (code-mesg cc))
                     (and (slot-boundp cc 'args) (code-args cc))))))

(define-condition case-error (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "`~s' evaluated to `~s', not one of [~@{`~s'~^ ~}]"))
  (:documentation "An error in a case statement.
This carries the function name which makes the error message more useful."))

(define-condition not-implemented (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "not implemented for ~a [~a]")
   (args :type list :reader code-args :initform
         (list (lisp-implementation-type) (lisp-implementation-version))))
  (:documentation "Your implementation does not support this functionality."))

;;;
;;; Extensions
;;;

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name)) (defun ,name ,arglist ,@body)))

(defmacro defcustom (name type init doc)
  "Define a typed variable."
  `(progn (declaim (type ,type ,name))
    (defvar ,name (the ,type ,init) ,doc)))

(defmacro defconst (name type init doc)
  "Define a typed constant."
  `(progn (declaim (type ,type ,name))
    (defconstant ,name (the ,type ,init) ,doc)))

(defmacro mk-arr (type init &optional len)
  "Make array with elements of TYPE, initializing."
  (if len `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
        :initial-contents ,init)))

(defmacro with-gensyms (syms &body body)
  "Bind symbols to gensyms.  First sym is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy) `(,sy (gensym ,(car syms)))) (cdr syms)))
    ,@body))

(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (with-gensyms ("MI-" mi)
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

(defun gc ()
  "Invoke the garbage collector."
  #+allegro (excl:gc)
  #+clisp (#+lisp=cl ext:gc #-lisp=cl lisp:gc)
  #+cmu (ext:gc)
  #+cormanlisp (cl::gc)
  #+gcl (si::gbc)
  #+lispworks (hcl:normal-gc)
  #+lucid (lcl:gc)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid)
  (error 'not-implemented :proc (list 'gc)))

(defun quit (&optional code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid)
  (error 'not-implemented :proc (list 'quit code)))

(defconst +eof+ cons (cons nil nil)
  "*The end-of-file object.
To be passed as the third arg to `read' and checked against using `eq'.")

(defun eof-p (stream)
  "Return T if the stream has no more data in it."
  (let ((cc (read-char stream nil nil)))
    (if cc (unread-char cc stream) t)))

(defun string-tokens (string &key (start 0) max)
  "Read from STRING repeatedly, starting with START, up to MAX tokens.
Return the list of objects read and the final index in STRING.
Binds `*package*' to the keyword package,
so that the bare symbols are read as keywords."
  (declare (type (or null fixnum) max) (type fixnum start))
  (let ((*package* (find-package "KEYWORD")))
    (if max
        (do ((beg start) obj res (num 0 (1+ num)))
            ((= max num) (values (nreverse res) beg))
          (declare (fixnum beg num))
          (setf (values obj beg)
                (read-from-string string nil +eof+ :start beg))
          (if (eq obj +eof+)
              (return (values (nreverse res) beg))
              (push obj res)))
        (read-from-string (concatenate 'string "(" string ")")
                          t nil :start start))))

#+cmu (progn
        (import '(ext:required-argument) :port)
        (export '(ext:required-argument) :port))
#-cmu (progn
        (proclaim '(ftype (function () nil) required-argument))
        (defun required-argument ()
          "A useful default for required arguments and DEFSTRUCT slots."
          (error "A required argument was not supplied.")))

;;;
;;; Function Compositions
;;;

(defmacro compose (&rest functions)
  "Macro: compose functions or macros of 1 argument into a lambda.
E.g., (compose abs (dl-val zz) 'key) ==>
  (lambda (yy) (abs (funcall (dl-val zz) (funcall key yy))))"
  (labels ((rec (xx yy)
             (let ((rr (list (car xx) (if (cdr xx) (rec (cdr xx) yy) yy))))
               (if (consp (car xx))
                   (cons 'funcall (if (eq (caar xx) 'quote)
                                      (cons (cadar xx) (cdr rr)) rr))
                   rr))))
    (with-gensyms ("COMPOSE-" arg)
      (let ((ff (rec functions arg)))
        `(lambda (,arg) ,ff)))))

(defun compose-f (&rest functions)
  "Return the composition of all the arguments.
All FUNCTIONS should take one argument, except for
the last one, which can take several."
  (reduce (lambda (f0 f1)
            (declare (function f0 f1))
            (lambda (&rest args) (funcall f0 (apply f1 args))))
          functions :initial-value #'identity))

(defun compose-all (&rest functions)
  "Return the composition of all the arguments.
All the values from nth function are fed to the n-1th."
  (reduce (lambda (f0 f1)
            (declare (function f0 f1))
            (lambda (&rest args) (multiple-value-call f0 (apply f1 args))))
          functions :initial-value #'identity))

(provide :ext)
;;; file ext.lisp ends here
