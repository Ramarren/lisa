;;;
;;; File: language.lisp
;;; Description: Macros that implement the LISA programming language.
;;;
;;; $Id: language.lisp,v 1.2 2000/10/12 02:39:45 youngde Exp $

(in-package "LISA")

(defmacro defrule (name &body body)
  `(redefine-defrule ',name ',body))

(defun redefine-defrule (name args)
  (labels ((variablep (sym)
             (eq (elt (symbol-name sym) 0) #\?))
           (extract-assignement (
           (lhs (ce-list lhs-forms)
             (let ((looking-at (first ce-list)))
               (cond ((null looking-at)
                      (error "Missing separator '=>'"))
                     ((stringp looking-at)
                      (if (stringp (first lhs-forms))
                          (error "Multiple doc strings.")
                        (lhs (rest ce-list) (append lhs-forms `(,looking-at)))))
                     ((consp looking-at)
                      (lhs (rest ce-list) (append lhs-forms looking-at)))
                     ((symbolp looking-at)
                      (cond ((eq looking-at '=>')
                             (values lhs-forms (rest ce-list)))
                            ((and (variablep looking-at)
                                  (eq (second ce-list '<-'
                             
                             (lhs (rest ce-list) (append lhs-forms `(,looking-at))))
                            (t (error "Missing variable."))))
                     (t (error "Malformed left-hand-side.")))))
           (rhs (
  (values name args))

#+ignore
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
