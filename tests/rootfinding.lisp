;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019, 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(defsuite root-finding (all-tests))

(deftest bisection-test (root-finding)
  "Test root-bisection on identity and cubic functions."
  (let ((*rootfinding-delta-relative* 1e-6)
        (*num=-tolerance* 1d-2))
    (assert-true (num= 0 (root-bisection #'identity (interval -1 2))))
    (assert-true (num= 5 (root-bisection (lambda (x)
                                           (expt (- x 5) 3))
                                         (interval -1 10))))))

