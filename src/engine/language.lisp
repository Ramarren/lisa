;;;
;;; File: language.lisp
;;; Description: Macros that implement the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.1 2000/10/10 20:42:29 youngde Exp $

(in-package "LISA")

(defmacro defrule (name &body body)
  `(redefine-defrule ',name ',body))

(defun redefine-defrule (name &rest args)
  (let* ((rule-body (first args))
         (doc-string (extract-documentation rule-body)))
    (labels ((lhs (ce-list lhs)
               (format t "lhs: working on ~S, looking at ~S~%"
                       ce-list (first ce-list))
               (let ((obj (first ce-list)))
                 (cond ((consp obj)
                        (lhs (rest ce-list) (append lhs obj)))
                       ((atom obj)
                        (when (eq obj '=>)
                          (format t "found lhs: ~S~%" lhs)
                          (values lhs (rest ce-list))))
                       ((null ce-list)
                        (error "Missing separator '=>'")))))
             (rhs (forms-list rhs)
               (let ((obj (first forms-list)))
                 (cond ((consp obj)
                        (rhs (rest forms-list) (append rhs obj)))
                       ((null obj)
                        (format t "found rhs: ~S~%" rhs)
                        (values rhs))))))
      (multiple-value-bind (lhs rest)
          (lhs (rest rule-body) nil)
        (values name lhs (rhs rest nil))))))

(defun extract-documentation (rule)
  (let ((doc (first rule)))
    (if (stringp doc)
        doc
      nil)))
