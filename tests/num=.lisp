;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
;;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(defsuite num= (all-tests))

(deftest num=-number-test (num=)
  "Test num= with scalar numbers."
  (let ((*num=-tolerance* 1e-3))
    (assert-true  (num= 1 1))
    (assert-true  (num= 1 1.0))
    (assert-true  (num= 1 1.001))
    (assert-false (num= 1 2))
    (assert-false (num= 1 1.01))))

(deftest num=-list-test (num=)
  "Test num= with lists."
  (let ((*num=-tolerance* 1e-3))
    (assert-true  (num= nil nil))
    (assert-true  (num= '(1) '(1.001)))
    (assert-true  (num= '(1 2) '(1.001 1.999)))
    (assert-false (num= '(0 1) '(0 1.02)))
    (assert-false (num= nil '(1)))))

(deftest num=-array-test (num=)
  "Test num= with arrays."
  (let* ((*num=-tolerance* 1e-3)
         (a #(0 1 2))
         (b #2A((0 1)
                (2 3))))
    (assert-true  (num= a a))
    (assert-true  (num= a #(0 1.001 2)))
    (assert-true  (num= a #(0 1.001 2.001)))
    (assert-true  (num= b b))
    (assert-true  (num= b #2A((0 1)
                              (2.001 3))))
    (assert-false (num= a b))
    (assert-false (num= a #(0 1)))
    (assert-false (num= a #(0 1.01 2)))
    (assert-false (num= b #2A((0 1))))
    (assert-false (num= b #2A((0 1.01)
                              (2 3))))))

(defstruct num=-test-struct
  "Structure for testing DEFINE-STRUCTURE-num=."
  a b)

(define-structure-num= num=-test-struct a b)

(deftest num=-structure-test (num=)
  "Test num= with structures."
  (let ((*num=-tolerance* 1e-3)
        (a (make-num=-test-struct :a 0 :b 1))
        (b (make-num=-test-struct :a "string" :b nil)))
    (assert-true  (num= a a))
    (assert-true  (num= a (make-num=-test-struct :a 0 :b 1)))
    (assert-true  (num= a (make-num=-test-struct :a 0 :b 1.001)))
    (assert-false (num= a (make-num=-test-struct :a 0 :b 1.01)))
    (assert-true  (num= b b))
    (assert-false (num= a b))))
