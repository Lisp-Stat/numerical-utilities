;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite root-finding
    :description "Tests root finding functions"
    :in all-tests)
(in-suite root-finding)

(test bisection-test
  (let ((*rootfinding-delta-relative* 1e-6)
        (*num=-tolerance* 1d-2))
    (is (num= 0 (root-bisection #'identity (interval -1 2))))
    (is (num= 5 (root-bisection (lambda (x)
				    (expt (- x 5) 3))
				  (interval -1 10))))))

