;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite interval
    :description "Test interval functions"
    :in all-tests)
(in-suite interval)

(test interval
  (let ((a (interval 1 2)))
    (is (num= 1 (interval-length a)))
    (is (num= 1.25 (interval-midpoint a 0.25)))
    (is (num= (interval 1.25 1.8) (shrink-interval a 0.25 0.2)))
    (is (in-interval? a 1.5))
    (is (in-interval? a 1))
    (is (in-interval? a 2))
    (not (in-interval? a 0.9))
    (not (in-interval? a 2.1))
    (signals error (interval 2 1))))

(test interval-hull
  (let ((a (interval 1 2)))
    (is (num= nil (interval-hull nil)))
    (is (num= a (interval-hull a)))
    (is (num= a (interval-hull '(1 1.5 2))))
    (is (num= a (interval-hull #(1 1.5 2))))
    (is (num= a (interval-hull #2A((1) (1.5) (2)))))
    (is (num= (interval -1 3)
	      (interval-hull (list (interval 0 2) -1 #(3) '(2.5)))))
    (signals error (interval-hull #C(1 2)))))

(test split-interval
  (let ((a (interval 10 20)))
    (is (num= (vector (interval 10 13) (interval 13 14) (interval 14 20))
	      (split-interval a (list (spacer 1) (relative 0.1) (spacer 2)))))
    (is (num= (vector (interval 10 16) (interval 16 20))
	      (split-interval a (list (spacer) 4))))
    (signals error (split-interval a (list 9)))
    (signals error (split-interval a (list 6 7 (spacer))))))

(test extendf-interval
  (let+ ((counter -1)
         (a (make-array 2 :initial-contents (list nil (interval 1 2)))))
    (extendf-interval (aref a (incf counter)) 3)
    (extendf-interval (aref a (incf counter)) 3)
    (is (num= (vector (interval 3 3) (interval 1 3)) a))
    (is (num= 1 counter))))

(test grid-in
    (is (num= #(0.0 0.5 1.0) (grid-in (interval 0.0 1.0) 3)))
    (is (num= #(0 2 4) (grid-in (interval 0 4) 3))))

(test subintervals-in
  (let ((expected (vector (interval 0 1 :open-left? nil :open-right? t)
                          (interval 1 2 :open-left? nil :open-right? t)
                          (interval 2 3 :open-left? nil :open-right? nil))))
    (is (num= (subintervals-in (interval 0 3) 3)
	      expected))))

(test plusminus-interval
  (is (num= (interval 0.5 1.5) (plusminus-interval 1 0.5))))
