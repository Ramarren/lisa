;;; This file is part of LISA, the Lisp-based Intelligent Software
;;; Agents platform.

;;; Copyright (C) 2000 David E. Young (de.young@computer.org)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: retrieve.lisp
;;; Description: Macros and functions implementing LISA's initial query
;;; language implementation.

;;; $Id: retrieve.lisp,v 1.20 2002/06/04 00:31:07 youngde Exp $

(in-package "LISA")

(defvar *query-result*
    '()
  "Holds the results of query firings.")

(defun run-query (query)
  "Runs a query (RULE instance), and returns both the value of *QUERY-RESULT*
  and the query name itself."
  (with-inference-engine ((current-engine))
    (let* ((?name (get-name query))
           (*query-result* '())
           (fact (assert (query-fact (query-name ?name)))))
      (run)
      (retract fact)
      (values *query-result* ?name))))

(defun define-query (name body)
  "The actual query definition function. Creates a RULE instance identified by
  the symbol NAME, with BODY as its LHS and RHS, and adds the new rule to the
  rete network. Returns the new RULE instance."
  (let ((rule (define-rule name body)))
    (add-rule (current-engine) rule)
    (values rule)))

(defmacro defquery (name &body body)
  "Defines a new query identified by the symbol NAME."
  `(define-query ,name ',body))

;;; Queries fired by RETRIEVE collect their results in the special variable
;;; *QUERY-RESULT*. As an example, one firing of this query, 
;;;
;;;   (retrieve (?x ?y) 
;;;     (?x (rocky (name ?name)))
;;;     (?y (hobbit (name ?name))))
;;;
;;; will produce a result similar to,
;;;
;;; (((?X . #<ROCKY @ #x7147b70a>) (?Y . #<HOBBIT @ #x7147b722>)))

(defmacro retrieve ((&rest varlist) &body body)
  "Issues a query against the knowledge base. First, the query body is
  normalized to produce a hash code. Next, the cache is checked to see if the
  query already exists; if it doesn't, then a new query (rule) is created,
  cached and added to the rete network. Finally, the query is run. Query
  results are collected into the variable *QUERY-RESULT* as the query rules
  fire. Each individual rule firing produces a list of CONS cells; for each
  cell, its CAR is a variable as found in VARLIST and its CDR is that
  variable's pattern binding. RETRIEVE returns two values; the value of
  *QUERY-RESULT* and the name of the rule implementing the query."
  (flet ((make-query-binding (var)
           `(cons ',var (instance-of-fact ,var))))
    (let ((query-name (gensym))
          (hash (gensym))
          (query (gensym)))
      `(let* ((,hash (sxhash (normalize-query ',body)))
              (,query-name (gensym))
              (,query (find-query ,hash)))
         (when (null ,query)
           (setf ,query
             (defquery ',query-name
                 (query-fact (query-name ,query-name))
               ,@body
               =>
               (push (list ,@(mapcar #'make-query-binding varlist))
                     *query-result*)))
           (remember-query ,hash ,query))
         (run-query ,query)))))

(defun forget-query (name)
  "Discards a query from the cache by 1) removing it from *QUERY-MAP*; and 2)
  removing it from the rete network."
  (flet ((remove-query (key name)
           (remhash key (get-query-table (current-engine)))
           (undefrule name (current-engine))))
    (block found
      (with-hash-table-iterator
          (next-item (get-query-table (current-engine)))
        (multiple-value-bind (foundp key value) (next-item)
          (when (and foundp
                     (string= (symbol-name value)
                              (symbol-name name)))
            (return-from found
              (remove-query key value))))))))

(defun remember-query (hash query)
  "Binds the name of QUERY (a RULE instance) to HASH by entering it into the
  hash table *QUERY-MAP*."
  (setf (gethash hash (get-query-table (current-engine))) (get-name query)))

(defun find-query (hash)
  "Looks up a query name in the local query table *QUERY-MAP*, indexed by
  HASH. Then, attempts to resolve that name to a rule instance in the current
  inference engine."
  (let ((query-name (gethash hash (get-query-table (current-engine)))))
    (if (not (null query-name))
        (progn
          (format t "Good. Found query in cache.~%")
          (find-rule (current-engine) query-name))
      (values nil))))

;;; NORMALIZE-QUERY transforms a query body like this: An incoming body,
;;;
;;;  ((?x (frodo (name ?name)))
;;;   (?y (ring-bearer (name ?name))))
;;;
;;; will normalize to,
;;;
;;;  (("#:?_1" "#:?_2" "FRODO" "NAME") 
;;;   ("#:?_2" "#:?_3" "NAME" "RING-BEARER"))
;;;
;;; Note how the variable relationships are preserved.

(defun normalize-query (body)
  "Takes a query body (BODY), which really just has the same structure as a
  rule LHS, and normalizes it such that semantically equivalent bodies will
  hash to the same value. Each pattern within the body is considered
  individually and transformed like this: 1) the pattern (a list) is
  flattened; 2) variables are replaced with symbols composed of a
  monotonically increasing integer, with their semantic relationships
  preserved; 3) other objects are replaced with their string representations;
  4) the pattern is sorted. The result is a list of 'flattened' patterns
  suitable for use as a hash key. See this function's source file for a
  concrete example."
  (let ((varlist '())
        (index 0))
    (labels ((map-variable (var)
               (let ((item (cdr (assoc var varlist))))
                 (when (null item)
                   (setf item
                     (make-symbol (format nil "?_~D" (incf index))))
                   (setf varlist (acons var item varlist)))
                 (values item)))
             (transform (obj)
               (cond ((variablep obj)
                      (prin1-to-string (map-variable obj)))
                     ((stringp obj) obj)
                     (t (prin1-to-string obj)))))
      (mapcar #'(lambda (pattern)
                  (sort (mapcar #'transform (flatten pattern))
                        #'string<))
              body))))

(defmacro with-simple-query ((var value) query &body body)
  "For each variable/instance pair in a query result, invoke BODY with VAR
  bound to the query variable and VALUE bound to the instance."
  (let ((result (gensym)))
    `(let ((,result ,query))
       (dolist (match ,result)
         (dolist (binding match)
           (let ((,var (car binding))
                 (,value (cdr binding)))
             ,@body))))))
