;; -*- Lisp -*-

(in-package "LESS.ENGINE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-unordered-pattern-ce
            add-slot
            examine)))

(defclass unordered-pattern-ce (conditional-element)
  ((slot-list :initform (make-array 1 :adjustable t
                                    :fill-pointer 0)
              :reader slot-list)))

(defmethod add-slot ((pattern unordered-pattern-ce) slot)
  (vector-push-extend slot (slot-list pattern)))

(defmethod examine ((ce unordered-pattern-ce) &optional (strm t) &key (indent 0))
  (let ((pad (+ 2 indent)))
    (format strm "~VA~S:~%" indent "" (get-class-name ce))
    (format strm "~VAname: ~S~%" pad "" (name ce))
    (with-accessors ((slots slot-list)) ce
      (dotimes (i (length slots) t)
        (examine (aref slots i) strm :indent pad)))))

(defun make-unordered-pattern-ce (name)
  (make-instance 'unordered-pattern-ce :name name))
