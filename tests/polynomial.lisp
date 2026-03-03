;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

;;; Although not used here, see:
;;; https://www.semanticscholar.org/paper/A-Simple-Test-Qualifying-the-Accuracy-of-Horner'S-Boldo-Daumas/fe5b9e5996947395680c9fb0c3dd918728e043fe
;;; For a methodology for testing Horner's rule for polynomials.

(defsuite polynomial (all-tests))

;;; Fixnum
(defvar polynomial-f1 (make-vector 'fixnum 2 -6 2 -1)) ; Answer:  5, for x = 3
(defvar polynomial-f2 (make-vector 'fixnum 2 0 3 1))   ; Answer: 23, for x = 2
(defvar polynomial-f3 (make-vector 'fixnum 1 3 5 7 9)) ; Answer: 83, for x = 2

;;; Single float
(defvar polynomial-s1 (make-vector 'single-float 2.0 -6.0 2.0 -1.0)) ; Answer:  5, for x = 3
(defvar polynomial-s2 (make-vector 'single-float 2.0 0.0 3.0 1.0))   ; Answer: 23, for x = 2
(defvar polynomial-s3 (make-vector 'single-float 1.0 3.0 5.0 7.0 9.0)) ; Answer: 83, for x = 2

;;; Double float
(defvar polynomial-d1 (make-vector 'double-float 2.0d0 -6.0d0 2.0d0 -1.0d0)) ; Answer:  5, for x = 3
(defvar polynomial-d2 (make-vector 'double-float 2.0d0 0.0d0 3.0d0 1.0d0))   ; Answer: 23, for x = 2
(defvar polynomial-d3 (make-vector 'double-float 1.0d0 3.0d0 5.0d0 7.0d0 9.0d0)) ; Answer: 83, for x = 2

;;; Bignum and everything else
(defvar polynomial-b1 #(2 0 1))
(defvar polynomial-b2 #(2 0 3 1))
(defvar polynomial-b3 #(1 3 5 7 9))

(deftest fixnum-polynomial (polynomial)
  "Test Horner's method of polynomial evaluation with fixnum coefficients."
  (let ((answer (evaluate-polynomial polynomial-f1 3)))
    (assert-eql 5 answer))
  (let ((answer (evaluate-polynomial polynomial-f2 2)))
    (assert-eql 23 answer))
  (let ((answer (evaluate-polynomial polynomial-f3 2)))
    (assert-eql 83 answer))
  (let ((answer (evaluate-polynomial (make-vector 'fixnum 5) 2)))
    (assert-eql 5 answer)))

(deftest single-float-polynomial (polynomial)
  "Test Horner's method of polynomial evaluation with single-float coefficients."
  (let ((answer (evaluate-polynomial polynomial-s1 3.0)))
    (assert-true (equal 5.0 answer)))
  (let ((answer (evaluate-polynomial polynomial-s2 2.0)))
    (assert-true (equal 23.0 answer)))
  (let ((answer (evaluate-polynomial polynomial-s3 2.0)))
    (assert-true (equal 83.0 answer))))

(deftest double-float-polynomial (polynomial)
  "Test Horner's method of polynomial evaluation with double-float coefficients."
  (let ((answer (evaluate-polynomial polynomial-d1 3.0d0)))
    (assert-true (equal 5.0d0 answer)))
  (let ((answer (evaluate-polynomial polynomial-d2 2.0d0)))
    (assert-true (equal 23.0d0 answer)))
  (let ((answer (evaluate-polynomial polynomial-d3 2.0d0)))
    (assert-true (equal 83.0d0 answer))))

(deftest untyped-polynomial (polynomial)
  "Test Horner's method of polynomial evaluation with untyped (bignum) coefficients."
  (let ((answer (evaluate-polynomial polynomial-b1 (1+ most-positive-fixnum))))
    (assert-eql 42535295865117307932921825928971026433 answer)))
