; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         regextst.lisp
; Description:  some tests for the regular expression compiler
; Author:       Joachim H. Laubsch
; Created:       9-Feb-93
; Modified:     Thu Oct  2 12:57:23 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      ZEBU
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: /home/ramarren/LISP/git-repos/lisa-tmp/lisa/contrib/zebu-3.5.5/win/test/Attic/regextst.lsp,v 1.1 2000/10/17 18:03:34 youngde Exp $
;
; (c) Copyright 1993, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: regextst.lsp,v $
; RCS Revision 1.1  2000/10/17 18:03:34  youngde
; RCS Returned Windows-specific stuff to Zebu
; RCS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "ZEBU")
(setq *regex-debug* nil)
(defun match-beginning (n)
  (first (AREF *REGEX-GROUPS* n)))

(defun match-end (n)
  (second (AREF *REGEX-GROUPS* n)))

(defun regex-test (n pat string result &key invert)
  (let ((form (regex-compile pat))
	*print-circle*)
    (princ ".")
    (if (eval `(let ((START 0) (END ,(LENGTH STRING)) (STRING ,string))
		. ,form))
	(let ((matched-string (subseq string 0 (match-end 0))))
	  (if (string= matched-string result)
	      (if invert
		  (warn "In ~S ~A did not match correctly" n pat)
		t)
	    (if invert
		t
	      (warn "In ~S ~A did not match correctly~%Only ~S was matched!"
		    n pat matched-string))))
      (if invert
	  t
	(warn "In ~S ~A did not compile correctly" n pat)))))
       
(regex-test 1 "\\(na\\)x+\\1" "naxna" "naxna")
(regex-test 2 "\\(na\\)x+\\1" "naxna123" "naxna")

(regex-test 3 "\\(na\\)x+" "naxxos" "naxx")
(regex-test 4 "\\(na\\)x+" "naxos" "nax")
(regex-test 5 "\\(na\\)x+" "naos" "na" :invert t)

(regex-test 6 "\\(na\\)x*" "naxxos" "naxx")
(regex-test 7 "\\(na\\)x*" "naxos" "nax")
(regex-test 8 "\\(na\\)x*" "naos" "na")

(regex-test 9 "[0-9]+" "123ab" "123")
(regex-test 10 "[a-zA-Z]+" "aAbb123" "aAbb")
(regex-test 11 "[0-9a-z]+" "1234&&*" "1234")
(regex-test 12 "[0-9a-z]+" "1234a&&*" "1234a")

(regex-test 13 "[0-9a-zA-Z]+" "a1234a" "a1234a")
(regex-test 14 "[0-9a-zA-Z&]+" "aAbb123&&*" "aAbb123&&")

(regex-test 15 "[0-9]+\\.[0-9]*" "0.123cm" "0.123")

(regex-test 16 "{[^}\\n]*}"
	    "{M.D. Harrison and A. Monk (Ed.)} \n\t foo: 2"
	    "{M.D. Harrison and A. Monk (Ed.)}")

(regex-test 17 "{[^}\\n]*}"
	    "{M.D. Harrison and
A. Monk (Ed.)} \n\t foo: 2"
	    "{M.D. Harrison and A. Monk (Ed.)}" :invert t)


(regex-test 18 "{[^}\\n]*}"
	    "{M.D. Harrison and {A. Monk} (Ed.)} \n\t foo: 2"
	    "{M.D. Harrison and {A. Monk} (Ed.)}" :invert t)

(regex-test 19 "ca?r" "car" "car")

(regex-test 20 "ca?r" "cr" "cr")

(regex-test 21 "c[ad]+r" "caaar" "caaar")

(regex-test 22 "c[ad]+r" "caaar aa1" "caaar")

(regex-test 23 "c[ad]+r$" "caaar" "caaar")

(regex-test 24 ".*" "" "")

(regex-test 25 ".*" "aa" "aa")

(regex-test 26 ".*" "aa" "aa")

(regex-test 27 "c[ad]?r" "cr" "cr")

(regex-test 28 "c[ad]?r" "car" "car")

(regex-test 29 "c[ad]?r" "cdr" "cdr")

(regex-test 30 "c[0-9]?r" "cr" "cr")

(regex-test 31 "c[0-9]?r" "c9rxx" "c9r")

(regex-test 32 "c[0-9]?r" "crxx" "cr")


;;(regex-test 27 "a\\|b" "a" "a")

(regex-test 33 "ab.yz" "ab yz" "ab yz")

(regex-test 34 "ab.yz" "ab
yz" "ab" :invert t)

(regex-test 35 "\\(abc\\)\\1" "abcabc" "abcabc")

(regex-test 36 "\\(abc\\)\\1x*\\(def\\)y*\\2" "abcabcxxxxdefyyyyyyydef$%%%%%"
	    "abcabcxxxxdefyyyyyyydef")

;;(regex-test 37 "a|bc*" "a" "a")

(let ((fn (def-regex-parser 'Natural_Number "[0-9]+")))
  (pprint fn)
  (compile (eval fn)))

(defun regex-test1 (number fn input output &optional invert)
  (let* ((match (funcall fn input))
	 (result (subseq input (match-beginning 0) (match-end 0)))
	 (test (and match
		    (> match 0)
		    (= (parse-integer result) output))))
    (if (if invert (not test) test) 
	(princ ".")
      (warn "wrong match in ~d: ~a found" number result))
    (values)))

(regex-test1 40 'Natural_Number "111" 111)

(regex-test1 41 'Natural_Number "111 af" 111)

(regex-test1 42 'Natural_Number "a111z" 0 t)

(let ((fn (def-regex-parser 'Natural_Number* "[0-9]*")))
  ;; (pprint fn)
  (eval fn))

(regex-test1 43 'Natural_Number* "111" 111)
(regex-test1 44 'Natural_Number* "111 af" 111)
(regex-test1 45 'Natural_Number* "a111z" 0 t)

(unless (equal (Natural_Number "11aab" 0 4) 2)
  (warn "No match"))

(unless (equal (Natural_Number "11aab" 1 4) 2)
  (warn "No match"))

(when (equal (Natural_Number "1aab" 1 4) 2)
  (warn "wrong match"))

(let ((fn (def-regex-parser 'd_seq "d+")))
  (eval fn))

(let ((fn (def-regex-parser 'd_seq* "d*")))
  (eval fn))

(eval (def-regex-parser 'Rest_of_line ".+\$"))
(let* ((s "abcdef") (n (length s)))
  (unless (equal (REST_OF_LINE s 1 n) n)
    (warn "Rest_of_line did not compile correctly")))

(eval (def-regex-parser 'Quotation-Rx "'[^']+'"))
(Quotation-Rx "'System 0x40147bb8 [sys_specs_Mfake]' provides no alternatives for allocating resource 'max_cpu_Rspu'")

(eval (def-regex-parser 'NatNumber "-?[0-9]+[^a-zA-Z/$+_.:]"))
(eval (def-regex-parser 'NatNumber "-?[0-9]+[^a-zA-Z]"))

(NATNUMBER "32mb_mem_array")

(regex-test 50 "[A-Z]+" "ABCY" "ABCY")

(regex-test 51 "[0-9]+\\.[0-9]*\\(e[+-]?[0-9]+\\)" "12.3e4  k" "12.3e4")
(regex-test 52 "[0-9]+\\.[0-9]*\\(e[+-]?[0-9]+\\)" "12.3e-4  k" "12.3e-4")
;;(regex-test 53 "[0-9]+\\.[0-9]*\\(e[+-]?[0-9]+\\)?" "12.3  k" "12.3")

(let ((fn (def-regex-parser 'foo "\\(a\\)\\1")))
  (pprint fn)
  (eval fn))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           End of regextst.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
