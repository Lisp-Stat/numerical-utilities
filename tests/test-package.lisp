;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(cl:defpackage #:num-utils-tests
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
        #:num-utils.matrix
        #:num-utils.matrix-shorthand
        #:num-utils.num=
        #:num-utils.quadrature
        #:num-utils.statistics
        #:num-utils.rootfinding
        #:num-utils.utilities)
  (:shadowing-import-from #:num-utils.statistics #:mean :variance #:median)
  (:export
   #:run))
