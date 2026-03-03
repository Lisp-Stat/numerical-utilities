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
