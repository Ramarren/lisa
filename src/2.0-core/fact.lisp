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

;;; File: fact.lisp
;;; Description:

;;; $Id: fact.lisp,v 1.3 2002/08/29 15:29:25 youngde Exp $

(in-package "LISA")

(defvar *show-hidden-fact-slots* nil)

(defclass fact ()
  ((name :initarg :name
         :reader fact-name
         :documentation
         "The symbolic name for the fact, as used in rules.")
   (id :initform -1
       :accessor fact-id
       :documentation
       "The numeric identifier assigned to the fact by the inference engine.")
   (slot-table :reader fact-slot-table
               :initform (make-hash-table)
               :documentation
               "A hash table holding the slots for this fact.")
   (meta-data :reader fact-meta-data
              :documentation
              "The META-FACT instance associated with this fact."))
  (:documentation
   "This class represents all facts in the knowledge base."))

(defun set-slot-value (fact slot-name value)
  "Assigns a new value to a slot in a fact and its associated CLOS
  instance. SLOT-NAME is a symbol; VALUE is the new value for the
  slot."
  (let* ((meta (fact-meta-data fact))
         (instance (find-instance-of-fact fact))
         (effective-slot (find-effective-slot meta slot-name)))
    (setf (slot-value instance effective-slot) value)
    (initialize-slot-value fact slot-name value)))

(defun initialize-slot-value (fact slot-name value)
  "Sets the value of a slot in a fact's slot table. FACT is a FACT instance;
  SLOT-NAME is a symbol; VALUE is the slot's new value."
  (setf (gethash slot-name (fact-slot-table fact)) value)
  fact)

(defun set-slot-from-instance (fact meta-fact instance slot-name)
  "Assigns to a slot the value from the corresponding slot in the fact's CLOS
  instance. FACT is a FACT instance; META-FACT is a META-FACT instance;
  INSTANCE is the fact's CLOS instance; SLOT-NAME is a symbol representing the
  affected slot."
  (initialize-slot-value
   fact slot-name
   (slot-value instance (find-effective-slot meta-fact slot-name))))

(defun get-slot-values (fact)
  "Returns a list of slot name / value pairs for every slot in a fact. FACT is
  a fact instance."
  (let ((slots (list)))
    (maphash #'(lambda (slot value)
                 (when (or (not (eq slot :object))
                           *show-hidden-fact-slots*)
                   (push (list slot value) slots)))
             (fact-slot-table fact))
    slots))

(defun get-slot-value (fact slot-name)
  "Returns the value associated with a slot name. FACT is a FACT instance;
  SLOT-NAME is a SLOT-NAME instance."
  (gethash slot-name (fact-slot-table fact)))

(defun effective-slot->slot-name (effective-slot-id)
  "Retrieves the symbolic name associated with a CLOS instance's
  slot. FACT is a FACT instance; EFFECTIVE-SLOT-ID is a symbol representing
  the effective name of a CLOS instance's slot."
  (let ((symbolic-slot-name
         (find-symbol (symbol-name effective-slot-id))))
    (cl:assert (not (null symbolic-slot-name)) nil
      "Slot name ~S has never been seen!" effective-slot-id)
    symbolic-slot-name))

(defun find-instance-of-fact (fact)
  "Retrieves the CLOS instance associated with a fact. FACT is a FACT
  instance."
  (get-slot-value fact :object))

(defun has-superclass (fact symbolic-name)
  (find symbolic-name (get-superclasses (fact-meta-data fact))))

(defun synchronize-with-instance (fact &optional (effective-slot nil))
  "Makes a fact's slot values and its CLOS instance's slot values match. If a
  slot identifier is provided then only that slot is synchronized. FACT
  is a FACT instance; EFFECTIVE-SLOT, if supplied, is a symbol representing
  the CLOS instance's slot."
  (let ((instance (find-instance-of-fact fact))
        (meta (fact-meta-data fact)))
    (flet ((synchronize-all-slots ()
             (mapc #'(lambda (slot-name)
                       (set-slot-from-instance 
                        fact meta instance slot-name))
                   (get-slot-list meta)))
           (synchronize-this-slot ()
             (set-slot-from-instance
              fact meta instance (effective-slot->slot-name effective-slot))))
      (if (null effective-slot)
          (synchronize-all-slots)
        (synchronize-this-slot)))
    fact))

(defun reconstruct-fact (fact)
  `(,(fact-name fact) ,@(get-slot-values fact)))

(defmethod print-object ((self fact) strm)
  (print-unreadable-object (self strm :type t :identity t)
    (let ((slots (get-slot-values self)))
      (format strm "F-~D ; ~S" (fact-id self) (fact-name self))
      (unless (null slots)
        (format strm " ; ~S" slots)))))

(defmethod initialize-instance :after ((self fact) &key (slots nil)
                                                        (instance nil))
  "Initializes a FACT instance. SLOTS is a list of slot name / value pairs,
  where (FIRST SLOTS) is a symbol and (SECOND SLOT) is the slot's
  value. INSTANCE is the CLOS instance to be associated with this FACT; if
  INSTANCE is NIL then FACT is associated with a template and a suitable
  instance must be created; otherwise FACT is bound to a user-defined class."
  (with-slots ((slot-table slot-table) (meta-data meta-data)) self
    (setf meta-data (find-meta-fact (fact-name self)))
    (mapc #'(lambda (slot-name)
              (setf (gethash slot-name slot-table) nil))
          (get-slot-list meta-data))
    (if (null instance)
        (initialize-fact-from-template self slots meta-data)
      (initialize-fact-from-instance self instance meta-data))
    self))

(defun initialize-fact-from-template (fact slots meta-data)
  "Initializes a template-bound FACT. An instance of the FACT's associated
  class is created and the slots of both are synchronized from the SLOTS
  list. FACT is a FACT instance; SLOTS is a list of symbol/value pairs."
  (let ((instance
         (make-instance (find-class (get-class-name meta-data) nil))))
    (cl:assert (not (null instance)) nil
      "No class was found corresponding to fact name ~S." (fact-name fact))
    (initialize-slot-value fact :object instance)
    (mapc #'(lambda (slot-spec)
              (let ((slot-name (first slot-spec))
                    (slot-value (second slot-spec)))
                (set-slot-value fact slot-name slot-value)))
          slots)
    fact))

(defun initialize-fact-from-instance (fact instance meta-data)
  "Initializes a fact associated with a user-created CLOS instance. The fact's
  slot values are taken from the CLOS instance. FACT is a FACT instance;
  INSTANCE is the CLOS instance associated with this fact."
  (mapc #'(lambda (slot-name)
            (set-slot-from-instance fact meta-data instance slot-name))
        (get-slot-list meta))
  (initialize-slot-value fact :object instance)
  fact)

(defun make-fact (name slots)
  "The default constructor for class FACT. NAME is the symbolic fact name as
  used in rules; SLOTS is a list of symbol/value pairs."
  (make-instance 'fact :name name :slots slots))

(defun make-fact-from-instance (name clos-instance)
  "A constructor for class FACT that creates an instance bound to a
  user-defined CLOS instance. NAME is the symbolic fact name; CLOS-INSTANCE is
  a user-supplied CLOS object."
  (make-instance 'fact :name name :instance clos-instance))
  
(defun make-fact-from-template (fact)
  "Creates a FACT instance using another FACT instance as a
  template. Basically a clone operation useful for such things as asserting
  DEFFACTS."
  (make-fact (fact-name fact)
             (mapcar #'(lambda (slot-name)
                         (list slot-name (get-slot-value fact slot-name)))
                     (get-slot-list (fact-meta-data fact)))))
