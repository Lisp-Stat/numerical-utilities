;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:num-utils
  (:nicknames :nu)
  (:documentation "Numerical utilities for Lisp-Stat")
  (:use :common-lisp)
  (:use-reexport #:num-utils.arithmetic
		 #:num-utils.chebyshev
		 #:num-utils.elementwise
		 #:num-utils.interval
		 #:num-utils.matrix
		 #:num-utils.num=
		 #:num-utils.utilities
		 #:num-utils.rootfinding
		 #:num-utils.polynomial
		 #:num-utils.test-utilities
		 #:num-utils.quadrature
		 #:num-utils.log-exp
		 #:num-utils.print-matrix))
