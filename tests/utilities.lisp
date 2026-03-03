;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019, 2022 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

(defsuite utilities (all-tests))

(deftest gethash (utilities)
  "Test gethash* errors on missing key."
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash 'a table) 1)
    (assert-eql 1 (gethash* 'a table))
    (assert-condition error (gethash* 'b table))))

(deftest biconditional (utilities)
  "Test bic (biconditional) predicate."
  (assert-true  (bic t t))
  (assert-true  (bic nil nil))
  (assert-false (bic t nil))
  (assert-false (bic nil t)))

(deftest splice-when (utilities)
  "Test splice-when and splice-awhen in backquote context."
  (assert-true (equal '(a b c) `(a ,@(splice-when t 'b) c)))
  (assert-true (equal '(a c)   `(a ,@(splice-when nil 'b) c)))
  (assert-true (equal '(a b c) `(a ,@(splice-awhen 'b it) c)))
  (assert-true (equal '(a c)   `(a ,@(splice-awhen (not 'b) it) c))))

(deftest with-double-floats (utilities)
  "Test with-double-floats coercion macro."
  (let ((a 1)
        (c 4)
        (d 5))
    (with-double-floats ((a 2)
                         (b a)
                         c
                         (d))
      (assert-true (= a 2d0))
      (assert-true (= b 1d0))
      (assert-true (= c 4d0))
      (assert-true (= d 5d0)))))

(deftest boolean (utilities)
  "Test simple-boolean-vector type and as-bit-vector."
  (let ((a #(nil nil t t))
        (b #(nil nil t 5))
        (c #*0011))
    (assert-true  (typep a 'simple-boolean-vector))
    (assert-false (typep b 'simple-boolean-vector))
    (assert-true  (equal c (as-bit-vector a)))))

;;; TODO (Papp): write tests for other utilities

