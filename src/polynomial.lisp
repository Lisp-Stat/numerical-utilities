;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.POLYNOMIAL -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils.polynomial)

;;; Expect optimisation warnings here for FIXNUM and T branches. T
;;; branch could probably be removed, as it covers relatively few use
;;; cases.
(declaim (inline evaluate-polynomial))
(defun evaluate-polynomial (coefficients x)
  "Return the sum of polynomials, weighted by COEFFICIENTS, at X.
COFFICIENTS are ordered from the highest degree down to the constant term.
X must be of the same type as COEFFICIENTS."
  (declare (optimize(speed 3)(safety 1)))
  (typecase x
    (double-float
     (let ((sum 0d0))
       (declare (double-float sum x)
		(simple-double-float-vector coefficients))
       (dotimes (index (the fixnum (length coefficients)))
	 (the double-float (setf sum (+ (aref coefficients index)
					(* x sum)))))
       (the double-float sum)))
    (single-float
     (let ((sum 0s0))
       (declare (single-float sum x)
		(simple-single-float-vector coefficients))
       (dotimes (index (the fixnum (length coefficients)))
	 (setf sum (+ (aref coefficients index)
		      (* x sum))))
       sum))
    (fixnum				; The usefulness of optimising this branch is doubtful,
					; since we cannot guarantee the result is a fixnum
     (let ((sum 0))
       (declare (fixnum sum x)
		(simple-fixnum-vector coefficients))
       (dotimes (index (the fixnum (length coefficients)))
	 (setf sum (+ (aref coefficients index)
		      (the fixnum (* x sum)))))
       sum))
    (t					; Here for completeness
     (let ((sum 0))
       (declare (vector coefficients))
       (dotimes (index (the fixnum (length coefficients)))
	 (setf sum (+ (aref coefficients index)
		      (* x sum))))
       sum))))

#+ignore
(defun evaluate-polynomial (coefficients x)
  "Return the sum of polynomials, weighted by the list of COEFFICIENTS, at X.
X and contents of COEFFICIENTS must be of the same type.
COEFFICIENTS are in descending order from the highest degree down to the constant term."
  (declare (optimize(speed 3)(safety 0))
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)) ; Function declared inline, but SBCL whines
	   ;; See https://groups.google.com/forum/#!msg/comp.lang.lisp/AsKvR0emFtU/ILVjmLTlJ50J
  (assert (and (plusp (length coefficients))
  	       (every (lambda (elt)
  			(typep elt (class-of x)))
  		      coefficients))
	  (coefficients x)
	  "Coefficients and X must be of the same type.")
  (typecase x
    (double-float
     (let((sum (car coefficients)))
       (declare (double-float sum x))
       (loop for i double-float in (cdr coefficients)
	  do (setf sum (+ i (* x sum))))
       sum))
    (single-float
     (let((sum (car coefficients)))
       (declare (single-float sum x))
       (loop for i single-float in (cdr coefficients)
	  do (setf sum (+ i (* x sum))))
       sum))
    (fixnum
     (let((sum (car coefficients)))
       (declare (fixnum sum x))
       (loop for i fixnum in (cdr coefficients)
	  do (setf sum (+ i (* x sum))))
       sum))
    (t
     (let((sum (car coefficients)))
       (loop for i in (cdr coefficients)
	  do (setf sum (+ i (* x sum))))
       sum))))


#| Implementation Notes

[1] From a discussion on the CCL mailing list, this was a message from
Stas Boukarev, one of the SBCL maintainers, when I asked about using
assert like this:

  (assert (and (plusp (length coefficients))
  	       (every (lambda (elt)
  			(typep elt (class-of x)))
  		      coefficients))
	  (coefficients x)
	  "Coefficients and X must be of the same type.")

so that I could declare the variables in the loop for evaluate-polynomial.

"Are you doing it for performance? Any performance gains you get from
declaring your variables in a loop will be destroyed by performing
typep at runtime.
The cost of determining type-of, parsing it and applying typep on it
is going to be very high, especially if it's done on every element of
a sequence.
If you do need to perform that operation, you can do
(defun foo (x sequence)
  (macrolet ((make-test (x types)
              `(etypecase ,x
                  ,@(loop for type in types
                          collect `(,type (lambda (x) (typep x ',type)))))))
    (every (make-test x (double-float single-float fixnum))
                      sequence)))"

Maybe even putting EVERY inside the expansion, to get better inlining.
And handle specialized arrays without going through each element.

|#
