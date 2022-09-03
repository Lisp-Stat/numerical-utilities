;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019, 2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

(def-suite utilities
    :description "Test utility functions"
    :in all-tests)
(in-suite utilities)

(test gethash
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash 'a table) 1)
    (is (= 1 (gethash* 'a table)))
    (signals error (gethash* 'b table))))

(test biconditional
  (is (bic t t))
  (is (bic nil nil))
  (not (bic t nil))
  (not (bic nil t)))

(test splice-when
  (is (equal '(a b c) `(a ,@(splice-when t 'b) c)))
  (is (equal '(a c) `(a ,@(splice-when nil 'b) c)))
  (is (equal '(a b c) `(a ,@(splice-awhen 'b it) c)))
  (is (equal '(a c) `(a ,@(splice-awhen (not 'b) it) c))))

(test with-double-floats
  (let ((a 1)
        (c 4)
        (d 5))
    (with-double-floats ((a 2)
                         (b a)
                         c
                         (d))
      (is (= a 2d0))
      (is (= b 1d0))
      (is (= c 4d0))
      (is (= d 5d0)))))

(test boolean
  (let ((a #(nil nil t t))
	(b #(nil nil t 5))
	(c #*0011))
    (is  (typep a 'simple-boolean-vector))
    (not (typep b 'simple-boolean-vector))
    (is  (equal c (as-bit-vector a)))))

;;; TODO (Papp): write tests for other utilities

