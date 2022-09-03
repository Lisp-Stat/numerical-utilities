;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.POLYNOMIAL -*-
;;; Copyright (c) 2019-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils.polynomial)

;;; If this turns out to have poor performance, see
;;; https://github.com/ruricolist/horner or the assembler version from
;;; Cephes

;;; Expect optimisation notes here for FIXNUM and T branches. T
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



;;;
;;; Evaluate ratios of polynomial functions
;;;

;;; See https://www.boost.org/doc/libs/1_68_0/libs/math/doc/html/math_toolkit/tuning.html
;;; https://en.wikipedia.org/wiki/Rational_function
;;; See: https://www.boost.org/doc/libs/1_76_0/boost/math/tools/rational.hpp"

;;; Note that the order of the coefficients here differs from
;;; evaluate-polynomial.  Here it is from the constant term up to the
;;; highest order polynomial.  This is because evaluate-polynomial was
;;; taken from Cephes, which orders coefficients highest->lowest, and
;;; evaluate-rational was taken from Boost, which orders them
;;; lowest->highest

(defun evaluate-rational (numerator denominator z)
  "Evaluate a rational function using Horner's method.  NUMERATOR and DENOMINATOR must be equal in size.  These always have a loop and so may be less efficient than evaluating a pair of polynomials.  However, there are some tricks we can use to prevent overflow that might otherwise occur in polynomial evaluation if z is large.  This is important in our Lanczos code for example.

N.B. The order of coefficients for this function is NOT the same as evaluate-polynomial. "
  (assert (= (length numerator)
	     (length denominator)) () "Numerator and denominator must be the same length")
  (let (s1 s2)
    (if (<= z 1)
	(progn
	  (setf s1 (last-elt numerator)
		s2 (last-elt denominator))
	  (loop for i from (- (length numerator) 2) downto 0
		do  (setf s1 (* s1 z)
			  s1 (+ s1 (aref numerator i))
			  s2 (* s2 z)
			  s2 (+ s2 (aref denominator i)))))
	(progn
	  (setf z (/ z)
		s1 (first-elt numerator)
		s2 (first-elt denominator))
	  (loop for i from 1 below (length numerator)
		do (setf s1 (* s1 z)
			  s1 (+ s1 (aref numerator i))
			  s2 (* s2 z)
			  s2 (+ s2 (aref denominator i))))))
    (/ s1 s2)))




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
