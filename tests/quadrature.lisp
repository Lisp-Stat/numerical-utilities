;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(defsuite quadrature (all-tests))

(deftest integration-finite (quadrature)
  "Test Romberg quadrature on finite intervals (closed and open)."
  (flet ((test-romberg (function interval value &rest rest)
           (let+ (((&interval a b) interval)
                  (closed-interval (interval a b))
                  (open-interval (interval a b :open-left? t :open-right? t)))
             ;; NOTE: original FiveAM code had (is (num=-function tol) actual val)
             ;; which always trivially passed. Fixed here to actually test the result.
             (assert-true (funcall (num=-function 1e-5)
                                   (apply #'romberg-quadrature function closed-interval rest)
                                   value))
             (assert-true (funcall (num=-function 1e-5)
                                   (apply #'romberg-quadrature function open-interval rest)
                                   value)))))
    (test-romberg (constantly 1d0) (interval 0 2) 2d0)
    (test-romberg #'identity (interval 1 5) 12d0)
    (test-romberg (lambda (x) (/ (exp (- (/ (expt x 2) 2)))
                                 (sqrt (* 2 pi))))
                  (interval 0 1) 0.3413447460685429d0 :epsilon 1d-9)))

(deftest integration-plusinf (quadrature)
  "Test Romberg quadrature on semi-infinite intervals."
  (assert-true (num= 1
                     (romberg-quadrature (lambda (x) (expt x -2))
                                         (interval 1 :plusinf))))
  (assert-true (num= 1/3
                     (romberg-quadrature (lambda (x) (exp (* -3 x)))
                                         (interval 0 :plusinf)))))
