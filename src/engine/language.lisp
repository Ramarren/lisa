;;;
;;; File: language.lisp
;;; Description: Macros that implement the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.3 2000/10/12 20:23:18 youngde Exp $

(in-package "LISA")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(defrule)))

(defmacro defrule (name &body body)
  (let ((rule (gensym)))
    (setf rule (stringify-defrule name body))
    `(zebu:read-parser ,rule
      :grammar (zebu:find-grammar "lisa"))))

(defun stringify-defrule (name body)
  "Convert a rule definition to string form for consumption by the
  Zebu parser."
  (with-output-to-string (strm)
    (format strm "(defrule ~A " name)
    (mapcar #'(lambda (tok)
                (format strm "~S " tok)) body)
    (format strm ")")))
