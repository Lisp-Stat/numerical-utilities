;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(defsuite interval (all-tests))

(deftest interval (interval)
  "Test basic interval construction and predicate."
  (let ((a (interval 1 2)))
    (assert-true (num= 1 (interval-length a)))
    (assert-true (num= 1.25 (interval-midpoint a 0.25)))
    (assert-true (num= (interval 1.25 1.8) (shrink-interval a 0.25 0.2)))
    (assert-true (in-interval? a 1.5))
    (assert-true (in-interval? a 1))
    (assert-true (in-interval? a 2))
    (assert-false (in-interval? a 0.9))
    (assert-false (in-interval? a 2.1))
    (assert-condition error (interval 2 1))))

(deftest interval-hull (interval)
  "Test interval-hull on various collections."
  (let ((a (interval 1 2)))
    (assert-true (num= nil (interval-hull nil)))
    (assert-true (num= a (interval-hull a)))
    (assert-true (num= a (interval-hull '(1 1.5 2))))
    (assert-true (num= a (interval-hull #(1 1.5 2))))
    (assert-true (num= a (interval-hull #2A((1) (1.5) (2)))))
    (assert-true (num= (interval -1 3)
                       (interval-hull (list (interval 0 2) -1 #(3) '(2.5)))))
    (assert-condition error (interval-hull #C(1 2)))))

(deftest split-interval (interval)
  "Test split-interval with spacer and relative specifications."
  (let ((a (interval 10 20)))
    (assert-true (num= (vector (interval 10 13) (interval 13 14) (interval 14 20))
                       (split-interval a (list (spacer 1) (relative 0.1) (spacer 2)))))
    (assert-true (num= (vector (interval 10 16) (interval 16 20))
                       (split-interval a (list (spacer) 4))))
    (assert-condition error (split-interval a (list 9)))
    (assert-condition error (split-interval a (list 6 7 (spacer))))))

(deftest extendf-interval (interval)
  "Test extendf-interval modifies intervals in place."
  (let+ ((counter -1)
         (a (make-array 2 :initial-contents (list nil (interval 1 2)))))
    (extendf-interval (aref a (incf counter)) 3)
    (extendf-interval (aref a (incf counter)) 3)
    (assert-true (num= (vector (interval 3 3) (interval 1 3)) a))
    (assert-eql 1 counter)))

(deftest grid-in (interval)
  "Test grid-in constructs evenly-spaced grids."
  (assert-true (num= #(0.0 0.5 1.0) (grid-in (interval 0.0 1.0) 3)))
  (assert-true (num= #(0 2 4)       (grid-in (interval 0 4) 3))))

(deftest subintervals-in (interval)
  "Test subintervals-in partition of an interval."
  (let ((expected (vector (interval 0 1 :open-left? nil :open-right? t)
                          (interval 1 2 :open-left? nil :open-right? t)
                          (interval 2 3 :open-left? nil :open-right? nil))))
    (assert-true (num= (subintervals-in (interval 0 3) 3)
                       expected))))

(deftest plusminus-interval (interval)
  "Test plusminus-interval constructor."
  (assert-true (num= (interval 0.5 1.5) (plusminus-interval 1 0.5))))
