;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:num-utils-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus
        #:fiveam
        #:select

        ;; num-utils subpackages (alphabetical order)
        #:num-utils.arithmetic
        #:num-utils.chebyshev
        #:num-utils.elementwise
        #:num-utils.interval
	#:num-utils.log-exp
        #:num-utils.matrix
        #:num-utils.matrix-shorthand
        #:num-utils.num=
        #:num-utils.polynomial
        #:num-utils.quadrature
        #:num-utils.rootfinding
	#:num-utils.test-utilities
	#:num-utils.norms
        #:num-utils.utilities)
  (:export #:run))
