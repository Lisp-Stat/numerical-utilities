;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(defsuite elementwise (all-tests))

(deftest elementwise-float-contagion (elementwise)
  "Test that elementwise-float-contagion returns the correct promoted type."
  (flet ((compare (type &rest objects)
           (type= (apply #'num-utils.elementwise::elementwise-float-contagion
                         objects) type)))
    (assert-true (compare 'double-float 1d0) 0)
    (assert-true (compare 'real 0 1))))

(deftest e-operations-tests (elementwise)
  "Test scalar and array element-wise arithmetic operations."
  (let+ (((&flet arr (dimensions element-type &rest elements)
            (aprog1 (make-array dimensions :element-type element-type)
              (assert (length= elements (array-total-size it)))
              (loop for index from 0
                    for element in elements
                    do (setf (row-major-aref it index)
                             (coerce element element-type))))))
         (a (arr '(2 3) 'double-float
                 1 2 3
                 4 5 6))
         (b (arr '(2 3) 'single-float
                 2 3 5
                 7 11 13)))
    (assert-equalp (arr '(2 3) 'double-float 3 5 8 11 16 19) (e+ a b))
    (assert-equalp (arr '(2 3) 'double-float 2 4 6 8 10 12)  (e* a 2s0))
    (assert-equalp (e+ (e+ a b) 2) (e+ a 2 b))
    (assert-equalp (e* a 2)        (e+ a a))
    (assert-condition error (e/ a 0))           ; division by 0
    (assert-condition error (e+ a               ; dimension incompatibility
                                (arr '(1 1) 'double-float 2)))
    (assert-equalp (e+ a 0)  (e+ a))
    (assert-equalp (e* a 1)  (e* a))
    (assert-equalp (e- 0d0 a) (e- a))
    (assert-equalp (e/ 1d0 a) (e/ a))
    (assert-true (num= #(1.0) (elog #(10) 10)))
    (assert-true (num= a (eexp (elog a))))))

;;; -----------------------------------------------------------------------
;;; US-012: Tier 1 trig unary tests
;;; -----------------------------------------------------------------------

(defsuite elementwise-trig (elementwise))

(deftest etan-scalar (elementwise-trig)
  "etan on a scalar equals tan."
  (assert-true (num= (tan 1.0d0) (etan 1.0d0))))

(deftest etan-array (elementwise-trig)
  "etan maps tan over each element of an array."
  (let* ((x #(0.0d0 0.5d0 1.0d0))
         (r (etan x)))
    (assert-true (num= (tan 0.0d0) (aref r 0)))
    (assert-true (num= (tan 0.5d0) (aref r 1)))
    (assert-true (num= (tan 1.0d0) (aref r 2)))))

(deftest easin-scalar (elementwise-trig)
  "easin on a scalar equals asin."
  (assert-true (num= (asin 0.5d0) (easin 0.5d0))))

(deftest easin-array (elementwise-trig)
  "easin maps asin over each element of an array (domain [-1, 1])."
  (let* ((x #(0.0d0 0.5d0 1.0d0))
         (r (easin x)))
    (assert-true (num= (asin 0.0d0) (aref r 0)))
    (assert-true (num= (asin 0.5d0) (aref r 1)))
    (assert-true (num= (asin 1.0d0) (aref r 2)))))

(deftest eacos-scalar (elementwise-trig)
  "eacos on a scalar equals acos."
  (assert-true (num= (acos 0.5d0) (eacos 0.5d0))))

(deftest eacos-array (elementwise-trig)
  "eacos maps acos over each element of an array (domain [-1, 1])."
  (let* ((x #(0.0d0 0.5d0 1.0d0))
         (r (eacos x)))
    (assert-true (num= (acos 0.0d0) (aref r 0)))
    (assert-true (num= (acos 0.5d0) (aref r 1)))
    (assert-true (num= (acos 1.0d0) (aref r 2)))))

(deftest e1atan-scalar (elementwise-trig)
  "e1atan on a scalar equals atan (1-arg form)."
  (assert-true (num= (atan 1.0d0) (e1atan 1.0d0))))

(deftest e1atan-array (elementwise-trig)
  "e1atan maps atan over each element of an array."
  (let* ((x #(0.0d0 1.0d0 2.0d0))
         (r (e1atan x)))
    (assert-true (num= (atan 0.0d0) (aref r 0)))
    (assert-true (num= (atan 1.0d0) (aref r 1)))
    (assert-true (num= (atan 2.0d0) (aref r 2)))))

(deftest esinh-scalar (elementwise-trig)
  "esinh on a scalar equals sinh."
  (assert-true (num= (sinh 1.0d0) (esinh 1.0d0))))

(deftest esinh-array (elementwise-trig)
  "esinh maps sinh over each element of an array."
  (let* ((x #(0.0d0 1.0d0 2.0d0))
         (r (esinh x)))
    (assert-true (num= (sinh 0.0d0) (aref r 0)))
    (assert-true (num= (sinh 1.0d0) (aref r 1)))
    (assert-true (num= (sinh 2.0d0) (aref r 2)))))

(deftest ecosh-scalar (elementwise-trig)
  "ecosh on a scalar equals cosh."
  (assert-true (num= (cosh 1.0d0) (ecosh 1.0d0))))

(deftest ecosh-array (elementwise-trig)
  "ecosh maps cosh over each element of an array."
  (let* ((x #(0.0d0 1.0d0 2.0d0))
         (r (ecosh x)))
    (assert-true (num= (cosh 0.0d0) (aref r 0)))
    (assert-true (num= (cosh 1.0d0) (aref r 1)))
    (assert-true (num= (cosh 2.0d0) (aref r 2)))))

(deftest etanh-scalar (elementwise-trig)
  "etanh on a scalar equals tanh."
  (assert-true (num= (tanh 1.0d0) (etanh 1.0d0))))

(deftest etanh-array (elementwise-trig)
  "etanh maps tanh over each element of an array."
  (let* ((x #(0.0d0 1.0d0 2.0d0))
         (r (etanh x)))
    (assert-true (num= (tanh 0.0d0) (aref r 0)))
    (assert-true (num= (tanh 1.0d0) (aref r 1)))
    (assert-true (num= (tanh 2.0d0) (aref r 2)))))

(deftest easinh-scalar (elementwise-trig)
  "easinh on a scalar equals asinh."
  (assert-true (num= (asinh 1.0d0) (easinh 1.0d0))))

(deftest easinh-array (elementwise-trig)
  "easinh maps asinh over each element of an array."
  (let* ((x #(0.0d0 1.0d0 2.0d0))
         (r (easinh x)))
    (assert-true (num= (asinh 0.0d0) (aref r 0)))
    (assert-true (num= (asinh 1.0d0) (aref r 1)))
    (assert-true (num= (asinh 2.0d0) (aref r 2)))))

(deftest eacosh-scalar (elementwise-trig)
  "eacosh on a scalar equals acosh (domain: x >= 1)."
  (assert-true (num= (acosh 2.0d0) (eacosh 2.0d0))))

(deftest eacosh-array (elementwise-trig)
  "eacosh maps acosh over each element of an array (values >= 1)."
  (let* ((x #(1.0d0 2.0d0 3.0d0))
         (r (eacosh x)))
    (assert-true (num= (acosh 1.0d0) (aref r 0)))
    (assert-true (num= (acosh 2.0d0) (aref r 1)))
    (assert-true (num= (acosh 3.0d0) (aref r 2)))))

(deftest eatanh-scalar (elementwise-trig)
  "eatanh on a scalar equals atanh (domain: |x| < 1)."
  (assert-true (num= (atanh 0.5d0) (eatanh 0.5d0))))

(deftest eatanh-array (elementwise-trig)
  "eatanh maps atanh over each element of an array (values in (-1, 1))."
  (let* ((x #(0.0d0 0.5d0 -0.5d0))
         (r (eatanh x)))
    (assert-true (num= (atanh  0.0d0) (aref r 0)))
    (assert-true (num= (atanh  0.5d0) (aref r 1)))
    (assert-true (num= (atanh -0.5d0) (aref r 2)))))

;;; -----------------------------------------------------------------------
;;; US-013: Tier 1 rounding and misc unary tests
;;; -----------------------------------------------------------------------

(defsuite elementwise-rounding (elementwise))

(deftest e1round-scalar (elementwise-rounding)
  "e1round on a scalar returns first value of round (banker's rounding)."
  (assert-true (num= 2 (e1round 2.5d0)))  ; 2.5 → 2 (round to even)
  (assert-true (num= 4 (e1round 3.5d0)))) ; 3.5 → 4 (round to even)

(deftest e1round-array (elementwise-rounding)
  "e1round maps round over each array element, capturing the quotient."
  (let* ((x #(0.5d0 1.5d0 2.5d0))
         (r (e1round x)))
    (assert-true (num= 0 (aref r 0)))  ; 0.5 → 0 (round to even)
    (assert-true (num= 2 (aref r 1)))  ; 1.5 → 2 (round to even)
    (assert-true (num= 2 (aref r 2))))) ; 2.5 → 2 (round to even)

(deftest e1truncate-scalar (elementwise-rounding)
  "e1truncate on a scalar returns first value of truncate."
  (assert-true (num=  2 (e1truncate  2.7d0)))
  (assert-true (num= -2 (e1truncate -2.7d0))))

(deftest e1truncate-array (elementwise-rounding)
  "e1truncate maps truncate over each array element."
  (let* ((x #(2.7d0 -2.7d0 0.1d0))
         (r (e1truncate x)))
    (assert-true (num=  2 (aref r 0)))
    (assert-true (num= -2 (aref r 1)))
    (assert-true (num=  0 (aref r 2)))))

(deftest esignum-scalar (elementwise-rounding)
  "esignum on a scalar equals signum."
  (assert-true (num=  1.0d0 (esignum  3.0d0)))
  (assert-true (num= -1.0d0 (esignum -3.0d0)))
  (assert-true (num=  0.0d0 (esignum  0.0d0))))

(deftest esignum-array (elementwise-rounding)
  "esignum maps signum over each array element."
  (let* ((x #(3.0d0 -3.0d0 0.0d0))
         (r (esignum x)))
    (assert-true (num=  1.0d0 (aref r 0)))
    (assert-true (num= -1.0d0 (aref r 1)))
    (assert-true (num=  0.0d0 (aref r 2)))))

;;; US-017: Tier 2 float-rounding tests (grouped with rounding suite)

(deftest e1ffloor-scalar (elementwise-rounding)
  "e1ffloor returns first value of ffloor, which is a float."
  (assert-true (num= -3.0d0 (e1ffloor -2.5d0)))
  (assert-true (floatp (e1ffloor 2.5d0))))

(deftest e1ffloor-array (elementwise-rounding)
  "e1ffloor maps ffloor over each array element."
  (let* ((x #(-2.5d0 -1.5d0 0.5d0))
         (r (e1ffloor x)))
    (assert-true (num= -3.0d0 (aref r 0)))
    (assert-true (num= -2.0d0 (aref r 1)))
    (assert-true (num=  0.0d0 (aref r 2)))))

(deftest e1fceiling-scalar (elementwise-rounding)
  "e1fceiling returns first value of fceiling, which is a float."
  (assert-true (num= 2.0d0 (e1fceiling 1.5d0)))
  (assert-true (floatp (e1fceiling 1.5d0))))

(deftest e1fround-scalar (elementwise-rounding)
  "e1fround returns first value of fround (float, banker's rounding)."
  (assert-true (num= 2.0d0 (e1fround 2.5d0)))  ; 2.5 → 2.0 (round to even)
  (assert-true (floatp (e1fround 2.5d0))))

(deftest e1ftruncate-scalar (elementwise-rounding)
  "e1ftruncate returns first value of ftruncate, which is a float."
  (assert-true (num=  2.0d0 (e1ftruncate  2.7d0)))
  (assert-true (num= -2.0d0 (e1ftruncate -2.7d0)))
  (assert-true (floatp (e1ftruncate 2.7d0))))

(deftest e2ffloor-scalar (elementwise-rounding)
  "e2ffloor on two scalars equals ffloor."
  (assert-true (num= (ffloor 7.0d0 3.0d0) (e2ffloor 7.0d0 3.0d0))))

(deftest e2fceiling-scalar (elementwise-rounding)
  "e2fceiling on two scalars equals fceiling."
  (assert-true (num= (fceiling 7.0d0 3.0d0) (e2fceiling 7.0d0 3.0d0))))

(deftest e2fround-scalar (elementwise-rounding)
  "e2fround on two scalars equals fround."
  (assert-true (num= (fround 7.0d0 3.0d0) (e2fround 7.0d0 3.0d0))))

(deftest e2ftruncate-scalar (elementwise-rounding)
  "e2ftruncate on two scalars equals ftruncate."
  (assert-true (num= (ftruncate 7.0d0 3.0d0) (e2ftruncate 7.0d0 3.0d0))))

;;; -----------------------------------------------------------------------
;;; US-014: Tier 1 binary op tests
;;; US-015: eatan wrapper tests
;;; -----------------------------------------------------------------------

(defsuite elementwise-binary (elementwise))

(deftest e2atan-scalar-scalar (elementwise-binary)
  "e2atan on two scalars equals atan(y, x)."
  (assert-true (num= (atan 1.0d0 1.0d0) (e2atan 1.0d0 1.0d0))))

(deftest e2atan-array-array (elementwise-binary)
  "e2atan maps atan(y, x) element-wise over two arrays."
  (let* ((y #(1.0d0 0.0d0 -1.0d0))
         (x #(1.0d0 1.0d0  1.0d0))
         (r (e2atan y x)))
    (assert-true (num= (atan  1.0d0 1.0d0) (aref r 0)))
    (assert-true (num= (atan  0.0d0 1.0d0) (aref r 1)))
    (assert-true (num= (atan -1.0d0 1.0d0) (aref r 2)))))

(deftest e2round-scalar (elementwise-binary)
  "e2round on two scalars equals round (a divided by b)."
  (assert-true (num= (round 5.0d0 2.0d0) (e2round 5.0d0 2.0d0))))

(deftest e2round-array-array (elementwise-binary)
  "e2round maps round over paired array elements."
  (let* ((a #(5.0d0 7.0d0 9.0d0))
         (b #(2.0d0 3.0d0 4.0d0))
         (r (e2round a b)))
    (assert-true (num= (round 5.0d0 2.0d0) (aref r 0)))
    (assert-true (num= (round 7.0d0 3.0d0) (aref r 1)))
    (assert-true (num= (round 9.0d0 4.0d0) (aref r 2)))))

(deftest e2truncate-scalar (elementwise-binary)
  "e2truncate on two scalars equals truncate (a divided by b)."
  (assert-true (num= (truncate 7.0d0 3.0d0) (e2truncate 7.0d0 3.0d0))))

(deftest e2floor-scalar (elementwise-binary)
  "e2floor on two scalars equals floor (a divided by b)."
  (assert-true (num= (floor 7.0d0 3.0d0) (e2floor 7.0d0 3.0d0))))

(deftest e2ceiling-scalar (elementwise-binary)
  "e2ceiling on two scalars equals ceiling (a divided by b)."
  (assert-true (num= (ceiling 7.0d0 3.0d0) (e2ceiling 7.0d0 3.0d0))))

(deftest erem-scalar (elementwise-binary)
  "erem on two scalars equals rem."
  (assert-true (num= (rem 7.0d0 3.0d0) (erem 7.0d0 3.0d0))))

(deftest erem-array-array (elementwise-binary)
  "erem maps rem over paired array elements."
  (let* ((a #(7.0d0 8.0d0 9.0d0))
         (b #(3.0d0 3.0d0 4.0d0))
         (r (erem a b)))
    (assert-true (num= (rem 7.0d0 3.0d0) (aref r 0)))
    (assert-true (num= (rem 8.0d0 3.0d0) (aref r 1)))
    (assert-true (num= (rem 9.0d0 4.0d0) (aref r 2)))))

(deftest e2/=-scalars (elementwise-binary)
  "e2/= on scalars returns T iff arguments are unequal."
  (assert-false (e2/= 1.0d0 1.0d0))
  (assert-true  (e2/= 1.0d0 2.0d0)))

(deftest e2/=-array-array (elementwise-binary)
  "e2/= maps /= over paired array elements."
  (let* ((a #(1.0d0 2.0d0 3.0d0))
         (b #(1.0d0 3.0d0 3.0d0))
         (r (e2/= a b)))
    (assert-false (aref r 0))  ; 1 /= 1 => NIL
    (assert-true  (aref r 1))  ; 2 /= 3 => T
    (assert-false (aref r 2)))) ; 3 /= 3 => NIL

(deftest e2-dimension-mismatch (elementwise-binary)
  "Binary elementwise ops signal error on dimension mismatch."
  (assert-condition error
    (e2+ (make-array 3 :initial-contents '(1.0d0 2.0d0 3.0d0))
         (make-array 2 :initial-contents '(1.0d0 2.0d0)))))

;;; US-015: eatan variadic wrapper

(deftest eatan-1-arg (elementwise-binary)
  "eatan with 1 argument dispatches to e1atan (scalar and array)."
  (assert-true (num= (atan 1.0d0) (eatan 1.0d0)))
  (let* ((x #(0.0d0 1.0d0 -1.0d0))
         (r (eatan x)))
    (assert-true (num= (atan  0.0d0) (aref r 0)))
    (assert-true (num= (atan  1.0d0) (aref r 1)))
    (assert-true (num= (atan -1.0d0) (aref r 2)))))

(deftest eatan-2-arg (elementwise-binary)
  "eatan with 2 arguments dispatches to e2atan (scalar and array)."
  (assert-true (num= (atan 1.0d0 1.0d0) (eatan 1.0d0 1.0d0)))
  (let* ((y #(1.0d0 0.0d0 -1.0d0))
         (x #(1.0d0 1.0d0  1.0d0))
         (r (eatan y x)))
    (assert-true (num= (atan  1.0d0 1.0d0) (aref r 0)))
    (assert-true (num= (atan  0.0d0 1.0d0) (aref r 1)))
    (assert-true (num= (atan -1.0d0 1.0d0) (aref r 2)))))

;;; -----------------------------------------------------------------------
;;; US-016: Tier 2 complex accessor tests
;;; -----------------------------------------------------------------------

(defsuite elementwise-complex (elementwise))

(deftest ephase-scalar (elementwise-complex)
  "ephase on a scalar equals phase."
  (assert-true (num= (phase #c(1.0d0 1.0d0)) (ephase #c(1.0d0 1.0d0))))
  (assert-true (num= (phase #c(1.0d0 0.0d0)) (ephase #c(1.0d0 0.0d0)))))

(deftest ephase-array (elementwise-complex)
  "ephase maps phase over each complex array element."
  (let* ((x (make-array 2 :initial-contents
                         (list #c(1.0d0 0.0d0) #c(0.0d0 1.0d0))))
         (r (ephase x)))
    (assert-true (num= (phase #c(1.0d0 0.0d0)) (aref r 0)))
    (assert-true (num= (phase #c(0.0d0 1.0d0)) (aref r 1)))))

(deftest erealpart-scalar (elementwise-complex)
  "erealpart on a scalar equals realpart."
  (assert-true (num= 3.0d0 (erealpart #c(3.0d0 4.0d0)))))

(deftest erealpart-array (elementwise-complex)
  "erealpart maps realpart over each complex array element."
  (let* ((x (make-array 2 :initial-contents
                         (list #c(1.0d0 2.0d0) #c(3.0d0 4.0d0))))
         (r (erealpart x)))
    (assert-true (num= 1.0d0 (aref r 0)))
    (assert-true (num= 3.0d0 (aref r 1)))))

(deftest eimagpart-scalar (elementwise-complex)
  "eimagpart on a scalar equals imagpart."
  (assert-true (num= 4.0d0 (eimagpart #c(3.0d0 4.0d0)))))

(deftest eimagpart-array (elementwise-complex)
  "eimagpart maps imagpart over each complex array element."
  (let* ((x (make-array 2 :initial-contents
                         (list #c(1.0d0 2.0d0) #c(3.0d0 4.0d0))))
         (r (eimagpart x)))
    (assert-true (num= 2.0d0 (aref r 0)))
    (assert-true (num= 4.0d0 (aref r 1)))))

(deftest ecis-scalar (elementwise-complex)
  "ecis on a scalar equals cis."
  (assert-true (num= (cis 0.0d0) (ecis 0.0d0))))

(deftest ecis-array (elementwise-complex)
  "ecis maps cis over each array element."
  (let* ((x #(0.0d0 1.0d0))
         (r (ecis x)))
    (assert-true (num= (cis 0.0d0) (aref r 0)))
    (assert-true (num= (cis 1.0d0) (aref r 1)))))

(deftest ecomplex-scalars (elementwise-complex)
  "ecomplex of two scalars equals complex."
  (assert-true (num= #c(3.0d0 4.0d0) (ecomplex 3.0d0 4.0d0))))

(deftest ecomplex-arrays (elementwise-complex)
  "ecomplex combines two real arrays element-wise into complex numbers."
  (let* ((re #(1.0d0 2.0d0 3.0d0))
         (im #(4.0d0 5.0d0 6.0d0))
         (r  (ecomplex re im)))
    (assert-true (num= #c(1.0d0 4.0d0) (aref r 0)))
    (assert-true (num= #c(2.0d0 5.0d0) (aref r 1)))
    (assert-true (num= #c(3.0d0 6.0d0) (aref r 2)))))

;;; -----------------------------------------------------------------------
;;; US-018: Tier 2 binary max/min tests
;;; -----------------------------------------------------------------------

(defsuite elementwise-reduction (elementwise))

(deftest e2max-scalar-scalar (elementwise-reduction)
  "e2max of two scalars returns the larger."
  (assert-true (num= 5.0d0 (e2max 3.0d0 5.0d0)))
  (assert-true (num= 5.0d0 (e2max 5.0d0 3.0d0))))

(deftest e2max-scalar-array (elementwise-reduction)
  "e2max broadcasts a scalar against each array element."
  (let* ((a #(1.0d0 3.0d0 5.0d0))
         (r (e2max 2.0d0 a)))
    (assert-true (num= 2.0d0 (aref r 0)))
    (assert-true (num= 3.0d0 (aref r 1)))
    (assert-true (num= 5.0d0 (aref r 2)))))

(deftest e2max-array-array (elementwise-reduction)
  "e2max maps max element-wise over two arrays."
  (let* ((a #(1.0d0 5.0d0 3.0d0))
         (b #(4.0d0 2.0d0 6.0d0))
         (r (e2max a b)))
    (assert-true (num= 4.0d0 (aref r 0)))
    (assert-true (num= 5.0d0 (aref r 1)))
    (assert-true (num= 6.0d0 (aref r 2)))))

(deftest e2min-scalar-scalar (elementwise-reduction)
  "e2min of two scalars returns the smaller."
  (assert-true (num= 3.0d0 (e2min 3.0d0 5.0d0)))
  (assert-true (num= 3.0d0 (e2min 5.0d0 3.0d0))))

(deftest e2min-array-array (elementwise-reduction)
  "e2min maps min element-wise over two arrays."
  (let* ((a #(1.0d0 5.0d0 3.0d0))
         (b #(4.0d0 2.0d0 6.0d0))
         (r (e2min a b)))
    (assert-true (num= 1.0d0 (aref r 0)))
    (assert-true (num= 2.0d0 (aref r 1)))
    (assert-true (num= 3.0d0 (aref r 2)))))



;;; Commented out by Papp. Should these be in array-operations?
;; (deftest (elementwise-tests)
;;   stack-tests
;;   (let ((a (array* '(2 3) t
;;                    1 2 3
;;                    4 5 6))
;;         (b (array* '(2 2) t
;;                    3 5
;;                    7 9))
;;         (*lift-equality-test* #'equalp))
;;     (assert-equalp (stack 'double-float :h a b)
;;                  (array* '(2 5) 'double-float
;;                          1 2 3 3 5
;;                          4 5 6 7 9))
;;     (assert-equalp (stack t :v (transpose a) b)
;;                  #2A((1 4)
;;                      (2 5)
;;                      (3 6)
;;                      (3 5)
;;                      (7 9)))
;;     (assert-equalp (stack 'fixnum :v a #(7 8 9) 10)
;;                  (array* '(4 3) 'fixnum
;;                           1 2 3
;;                           4 5 6
;;                           7 8 9
;;                           10 10 10))
;;     (assert-equalp (stack t :h b #(1 2) b 9 b)
;;                  (array* '(2 8) t
;;                          3 5 1 3 5 9 3 5
;;                          7 9 2 7 9 9 7 9))
;;     (assert-equalp (stack t :h
;;                         (vector* 'double-float 1d0 2d0)
;;                         (vector* 'double-float 3d0 4d0))
;;                  (array* '(2 2) 'double-float
;;                          1 3
;;                          2 4))
;;     (assert-equalp (stack 'double-float :h 1.0d0 #()) ; empty array
;;                  (array* '(0 2) 'double-float))))

;; (deftest (elementwise-tests)
;;   concat-test
;;   (assert-equalp (concat t #(1 2 3) #(4 5 6) (list  7) '(8 9 10))
;;                (numseq 1 10 :type t) :test #'equalp))
