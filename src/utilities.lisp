;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.UTILITIES -*-
;;; Copyright (c) 2019-2023, 2025 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:num-utils.utilities
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export #:gethash*
	   #:splice-when
	   #:splice-awhen
	   #:curry*
	   #:check-types
	   #:define-with-multiple-bindings
	   #:within?
	   #:fixnum?
	   #:simple-fixnum-vector
	   #:simple-single-float-vector
	   #:as-simple-fixnum-vector
	   #:simple-boolean-vector
	   #:as-bit-vector
	   #:as-double-float
	   #:with-double-floats
	   #:as-simple-double-float-vector
	   #:simple-double-float-vector
	   #:make-vector
	   #:generate-sequence
	   #:expanding
	   #:bic
	   #:binary-search
	   #:as-alist
	   #:as-plist)
  (:documentation "A collection of utilities to work with floating point values. Optimised for double-float. Provides type conversion functions, vector creation utilities, sequence generation, binary search, and utility macros including currying, multiple bindings, and conditional splicing. Features specialized array types for fixnum, boolean, and floating-point vectors with conversion functions."))
(in-package #:num-utils.utilities)

(defmacro gethash* (key hash-table
                    &optional (datum "Key not found.")
                    &rest arguments)
  "Like GETHASH, but checking that KEY is present and raising the given error if not."
  (with-unique-names (value present?)
    `(multiple-value-bind (,value ,present?) (gethash ,key ,hash-table)
       (assert ,present? () ,datum ,@arguments)
       ,value)))

(defmacro splice-when (test &body forms)
  "Similar to when, but wraps the result in list.

Example: `(,foo ,@(splice-when add-bar? bar))"
  `(when ,test
     (list
      (progn ,@forms))))

(defmacro splice-awhen (test &body forms)
  "Similar to splice-when, but binds IT to test."
  `(awhen ,test
     (list
      (progn ,@forms))))

(defmacro curry* (function &rest arguments)
  "Currying in all variables that are not *.  Note that this is a macro, so * should not be quoted, and FUNCTION will be used as is, ie it can be a LAMBDA form."
  (let ((arguments (loop for arg in arguments
                         collect (cond
                                   ((eq arg '*) (gensym "ARG"))
                                   ((keywordp arg) (list arg))
                                   (t (list (gensym "VAR") arg))))))
    `(let ,(loop for arg in arguments
                 when (and (consp arg) (cdr arg))
                 collect arg)
       (lambda ,(loop for arg in arguments
                      unless (consp arg)
                      collect arg)
         (,function ,@(loop for arg in arguments
                            collect (if (consp arg)
                                        (car arg)
                                        arg)))))))

(defmacro check-types ((&rest arguments) type)
  "CHECK-TYPE for multiple places of the same type.  Each argument is either a place, or a list of places and a type-string.

Example: (check-types (a b) double-float)"
  `(progn
     ,@(loop
         for argument :in arguments
         collecting (if (atom argument)
                        `(check-type ,argument ,type)
                        (let+ (((place type-string) argument))
                          `(check-type ,place ,type ,type-string))))))

(defmacro define-with-multiple-bindings
    (macro &key
           (plural (intern (format nil "~aS" macro)))
           (docstring (format nil "Multiple binding version of ~(~a~)." macro)))
  "Define a version of MACRO with multiple arguments, given as a list.  Application of MACRO will be nested.  The new name is the plural of the old one (generated using format by default)."
  `(defmacro ,plural (bindings &body body)
     ,docstring
     (if bindings
         `(,',macro ,(car bindings)
                    (,',plural ,(cdr bindings)
			       ,@body))
         `(progn ,@body))))

(declaim (inline within?))
(defun within? (left value right)
  "Return non-nil iff value is in [left,right)."
  (and (<= left value) (< value right)))


;;; fixnum
(declaim (inline fixnum?))
(defun fixnum? (object)
  "Check of type of OBJECT is fixnum."
  (typep object 'fixnum))

(deftype simple-fixnum-vector ()
  "Simple vector of fixnum elements."
  '(simple-array fixnum (*)))

(defun as-simple-fixnum-vector (sequence &optional copy?)
  "Convert SEQUENCE to a SIMPLE-FIXNUM-VECTOR.  When COPY?, make sure that they don't share structure."
  (if (and (typep sequence 'simple-fixnum-vector) copy?)
      (copy-seq sequence)
      (coerce sequence 'simple-fixnum-vector)))


;;; boolean
(declaim (inline boolean?))
(defun boolean? (object)
  "Check type of OBJECT is BOOLEAN."
  (typep object 'boolean))

(defun boolean-sequence-p (x)
  (every #'boolean? x))

(deftype simple-boolean-vector (&optional (length '*))
  "Vector of BOOLEAN elements."
  `(and (simple-array * (,length))
	(satisfies boolean-sequence-p)))

(defun as-bit-vector (v)
  "Return a bit vector where each non-nil element of V is mapped to 1 and each NIL element is mapped to 0"
  (map 'simple-bit-vector #'(lambda (x) (if x 1 0)) v))


;;; double-float
(defun as-double-float (x)
  "Convert argument to DOUBLE-FLOAT."
  (coerce x 'double-float))

(defmacro with-double-floats (bindings &body body)
  "For each binding = (variable value), coerce VALUE to DOUBLE-FLOAT and bind it to VARIABLE for BODY.  When VALUE is omitted, VARIABLE is used instead.  When BINDING is an atom, it is used for both the value and the variable.

Example:
  (with-double-floats (a
                       (b)
                       (c 1))
    ...)"
  `(let ,(mapcar (lambda (binding)
                   (let+ (((variable &optional (value variable))
                           (ensure-list binding)))
                     `(,variable (as-double-float ,value))))
                 bindings)
     ,@body))

(deftype simple-double-float-vector (&optional (length '*))
  "Simple vector of double-float elements."
  `(simple-array double-float (,length)))

(deftype simple-single-float-vector (&optional (length '*))
  "Simple vector of single-float elements."
  `(simple-array single-float (,length)))

(defun as-simple-double-float-vector (sequence &optional copy?)
  "Convert SEQUENCE to a SIMPLE-DOUBLE-FLOAT-VECTOR.  When COPY?, make sure they don't share structure."
  (assert (every #'realp sequence) (sequence) "SEQUENCE ~S contains non-numeric values." sequence)
  (if (and (typep sequence 'simple-double-float-vector) copy?)
      (copy-seq sequence)
      (map 'simple-double-float-vector 'as-double-float sequence)))



(defun generate-sequence (result-type size function)
  "Like MAKE-SEQUENCE, but using a function to fill the result.

Example to create a sequence of random numbers between 0-1 from the uniform distribution:
(generate-sequence '(vector double-float) 100 (lambda () (random 1.0))).
Essentially the initial values are ignored when using this function.
See also: aops:generate"
  (map-into (make-sequence result-type size) function))

(defmacro expanding (&body body)
  "Expand BODY. Useful for generating code programmatically."
  (with-gensyms (local-macro)
    `(macrolet ((,local-macro ()
                  ,@body))
       (,local-macro))))

(defun bic (a b)
  "Biconditional.  Returns A <=> B."
  (if a b (not b)))

(defun binary-search (sorted-reals value)
  "Return INDEX such that

  (WITHIN? (AREF SORTED-REALS INDEX) VALUE (AREF SORTED-REALS (1+ INDEX)).

SORTED-REALS is assumed to be reals sorted in ascending order (not checked, if this does not hold the result may be nonsensical, though the algorithm will terminate).

If value is below (or above) the first (last) break, NIL (T) is returned."
  (let+ ((left 0)
         (right (1- (length sorted-reals)))
         ((&flet sr (index) (aref sorted-reals index))))
    (cond
      ((< value (sr left)) nil)
      ((<= (sr right) value) t)
      (t (loop
           (when (= (1+ left) right)
             (return left))
           (let ((middle (floor (+ left right) 2)))
             (if (< value (sr middle))
                 (setf right middle)
                 (setf left middle))))))))

(defgeneric as-alist (object)
  (:documentation "Return OBJECT as an ALIST.  Semantics depends on OBJECT."))

(defgeneric as-plist (object)
  (:documentation "Return OBJECT as a PLIST.  Semantics depends on OBJECT.  The default method uses AS-ALIST.")
  (:method (object)
    (alist-plist (as-alist object))))

(defun make-vector (element-type &rest initial-contents)
  (make-array (length initial-contents) :element-type element-type
              :initial-contents initial-contents))

(define-compiler-macro make-vector (element-type &rest initial-contents)
  `(let ((vec (make-array ,(length initial-contents)
                          :element-type ,element-type)))
     ,@(let ((i -1))
	    (mapcar (lambda (form)
		      `(setf (aref vec ,(incf i)) ,form))
		    initial-contents))
     vec))

