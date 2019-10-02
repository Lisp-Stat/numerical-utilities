;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite arithmetic
    :description "Test arithmetic functions"
    :in all-tests)
(in-suite arithmetic)

(test arithmetic-functions
  (is (same-sign? 1 2 3))
  (not (same-sign? 1 -2 3))
  (is (= 4 (square 2)))
  (is (= 4.0 (absolute-square 2.0)))
  (is (= 25 (absolute-square #C(3 4))))
  (is (= 2 (abs-diff 3 5)))
  (is (= 2 (abs-diff -3 -5)))
  (is (num= 2 (log10 100)))
  (is (num= 8 (log2 256)))
  (is (= 1/5 (1c 4/5)))
  (is (divides? 8 2))
  (not (divides? 8 3))
  (is (= 2 (as-integer 2.0)))
  (signals error (as-integer 2.5)))

(test arithmetic-sequences
  (is (equalp #(2 3 4)   (numseq 2 4)))
  (is (equalp #(2 4 6 8) (numseq 2 nil :length 4 :by 2)))
  (is (equalp #(0 1 2 3) (ivec 4)))
  (is (equalp #(1 2 3)   (ivec 1 4)))
  (is (equalp #(1 3)     (ivec 1 4 2)))
  (signals error #(1 3)  (ivec 4 1 1 t)))

(test arithmetic-summaries
  (let ((v #(2 3 4)))
    (is (= 9 (sum v)))
    (is (= 24 (product v)))
    (is (equalp #(2 5 9) (cumulative-sum v)))
    (is (equalp #(2 6 24) (cumulative-product v)))
    (is (= 0 (sum #())))
    (is (= 1 (product #())))
    (is (equalp #() (cumulative-sum #())))
    (is (equalp #() (cumulative-product #())))))

(test norms
  (let* ((a #(2 3 4))
         (a-list (coerce a 'list))
         (b #(#C(3 4) 0 5 5 5))
         (b-list (coerce b 'list)))
    (is (num= (sqrt 29) (l2norm a)))
    (is (num= (sqrt 29) (l2norm a-list)))
    (is (num= 10 (l2norm b)))
    (is (num= 10 (l2norm b-list)))))

(test normalize-probabilities
  (let* ((a (vector 1 2 7))
         (a-copy (copy-seq a)))
    (is (equalp #(1/10 2/10 7/10) (normalize-probabilities a)))
    (is (equalp a a-copy))            ; not modified
    (is (equalp #(0.1d0 0.2d0 0.7d0)
		(normalize-probabilities a :element-type 'double-float)))
    (is (equalp a a-copy))            ; not modified
    (signals error (normalize-probabilities #(1 -1)))
    (let ((normalized #(0.1d0 0.2d0 0.7d0)))
      (is (equalp normalized
		  (normalize-probabilities a
                                   :element-type 'double-float
                                   :result nil)))
      (is (equalp a normalized))
      (not (equalp a a-copy)))))

(test arithmetic-rounding
  (is (equalp '(25 2) (multiple-value-list (floor* 27 5))))
  (is (equalp '(26 1) (multiple-value-list (floor* 27 5 1))))
  (is (equalp '(30 -3) (multiple-value-list (ceiling* 27 5))))
  (is (equalp '(31 -4) (multiple-value-list (ceiling* 27 5 1))))
  (is (equalp '(25 2) (multiple-value-list (round* 27 5))))
  (is (equalp '(29 -2) (multiple-value-list (round* 27 5 -1))))
  (is (equalp '(-25 -2) (multiple-value-list (truncate* -27 5))))
  (is (equalp '(-24 -3) (multiple-value-list (truncate* -27 5 1)))))

