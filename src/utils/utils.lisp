;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: utils.lisp
;;; Description: Miscellaneous utility functions.

;;; $Id: utils.lisp,v 1.13 2001/01/23 21:05:00 youngde Exp $

(in-package :lisa)

(defun find-before (item sequence &key (test #'eql))
  "Returns both that portion of SEQUENCE that occurs before ITEM and
  the rest of SEQUENCE anchored at ITEM, or NIL otherwise."
  (labels ((find-item (obj seq test val)
             (let ((item (first seq)))
               (cond ((null seq)
                      (values nil nil))
                     ((funcall test obj item)
                      (values val seq))
                     (t
                      (find-item obj (rest seq) test (nconc val `(,item))))))))
    (find-item item sequence test nil)))

(defun find-after (item sequence &key (test #'eql))
  "Returns that portion of SEQUENCE that occurs after ITEM, or NIL
  otherwise."
  (cond ((null sequence)
         (values nil))
        ((funcall test item (first sequence))
         (rest sequence))
        (t (find-after item (rest sequence) :test test))))

(defun find-if-after (predicate sequence)
  (cond ((null sequence)
         (values nil))
        ((funcall predicate (first sequence))
         (rest sequence))
        (t
         (find-if-after predicate (rest sequence)))))

(defun compare2 (predicate lst1 lst2)
  "Applies PREDICATE to corresponding pairs of elements in both lists, and
  returns T if PREDICATE was true for each pair, or NIL as soon as
  PREDICATE fails."
  (declare (type list lst1) (type list lst2))
  (cond ((not (and (listp lst1)
                   (listp lst2)))
         (values nil))
        ((and (null lst1) (null lst2))
         (values t))
        ((or (and (null lst1) (not (null lst2)))
             (and (null lst2) (not (null lst1))))
         (values nil))
        (t
         (if (apply predicate `(,(first lst1) ,(first lst2)))
             (compare2 predicate (rest lst1) (rest lst2))
           (values nil)))))

(defun make-interned-symbol (&rest args)
  (intern (make-symbol (apply #'format nil args))))

(defun lsthash (func ht)
  "Applies FUNC to each entry in hashtable HT and, if FUNC so
  indicates, appends the object to a LIST. If NIL is an acceptible
  object, then FUNC should return two values; NIL and T."
  (let ((seq (list)))
    (maphash #'(lambda (key val)
                 (multiple-value-bind (obj use-p)
                     (funcall func key val)
                   (unless (and (null obj)
                                (not use-p))
                     (push obj seq)))) ht)
    (nreverse seq)))

(defun collect (predicate list)
  (let ((collection (list)))
    (dolist (obj list)
      (when (funcall predicate obj)
        (push obj collection)))
    (nreverse collection)))

;;; Courtesy of Paul Graham...

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
