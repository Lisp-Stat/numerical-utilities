;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(cl:defpackage #:num-utils.utilities
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:gethash*
   #:splice-when
   #:splice-awhen
   #:curry*
   #:check-types
   #:define-with-multiple-bindings
   #:unlessf
   #:within?
   #:fixnum?
   #:simple-fixnum-vector
   #:simple-single-float-vector
   #:as-simple-fixnum-vector
   #:as-double-float
   #:with-double-floats
   #:simple-double-float-vector
   #:make-vector
   #:generate-sequence
   #:expanding
   #:bic
   #:binary-search
   #:sequencep
   #:as-alist
   #:as-plist))

(defpackage #:num-utils.arithmetic
  (:use #:cl
        #:alexandria-2
        #:anaphora
        #:num-utils.utilities
        #:let-plus)
  (:export
   #:multf
   #:same-sign-p
   #:square
   #:absolute-square
   #:abs-diff
   #:log10
   #:log2
   #:1c
   #:divides?
   #:as-integer
   #:numseq
   #:ivec
   #:sum
   #:product
   #:cumulative-sum
   #:cumulative-product
   #:l2norm-square
   #:l2norm
   #:normalize-probabilities
   #:floor*
   #:ceiling*
   #:round*
   #:truncate*
   #:sequence-maximum
   #:sequence-minimum))

(defpackage #:num-utils.num=
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:num-delta
   #:*num=-tolerance*
   #:num=
   #:num=-function
   #:define-num=-with-accessors
   #:define-structure-num=))

(defpackage #:num-utils.interval
  (:use #:cl
        #:alexandria
        #:anaphora
        #:num-utils.num=
        #:num-utils.utilities
        #:let-plus)
  (:export
   #:left
   #:open-left?
   #:right
   #:open-right?
   #:&interval
   #:interval
   #:finite-interval
   #:plusinf-interval
   #:minusinf-interval
   #:real-line
   #:plusminus-interval
   #:interval-length
   #:interval-midpoint
   #:in-interval?
   #:extend-interval
   #:extendf-interval
   #:interval-hull
   #:relative
   #:spacer
   #:split-interval
   #:shrink-interval
   #:grid-in
   #:subintervals-in
   #:shift-interval))

(defpackage #:num-utils.chebyshev
  (:use #:cl
        #:alexandria
        #:anaphora
        #:num-utils.interval
        #:num-utils.utilities
        #:let-plus)
  (:export ; These should probably be renamed in verb-object form
   #:chebyshev-root
   #:chebyshev-roots
   #:chebyshev-regression
   #:evaluate-chebyshev
   #:chebyshev-approximate))

(defpackage #:num-utils.polynomial
  (:use #:cl
        #:alexandria
        #:num-utils.utilities)
  (:nicknames #:poly)
  (:export #:evaluate-polynomial))

(cl:defpackage #:num-utils.elementwise
  (:use #:cl
        #:alexandria
        #:num-utils.arithmetic
        #:num-utils.utilities
        #:let-plus)
  (:nicknames #:elmt)			;num-util elementwise mathmatics
  (:export
   #:elementwise-float-contagion
   #:e+
   #:e-
   #:e*
   #:e/
   #:e2+
   #:e2-
   #:e2*
   #:e2/
   #:e1-
   #:e1/
   #:e2log
   #:e2exp
   #:e1log
   #:e1exp
   #:eexpt
   #:eexp
   #:elog
   #:esqrt
   #:efloor
   #:eceiling
   #:econjugate
   #:ereduce
   #:emin
   #:emax))

(defpackage #:num-utils.extended-real
  (:use #:cl #:alexandria)
  (:nicknames #:xreal)
  (:shadow #:= #:< #:> #:<= #:>=)
  (:export
   :infinite?
   :extended-real
   :=
   :<
   :>
   :<=
   :>=
   :plusinf
   :minusinf
   :with-template
   :lambda-template))

(cl:defpackage #:num-utils.print-matrix
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:print-length-truncate
   #:*print-matrix-precision*
   #:print-matrix))

(cl:defpackage #:num-utils.matrix
  (:use #:cl
        #:alexandria
        #:anaphora
        #:num-utils.elementwise
        #:num-utils.num=
        #:num-utils.print-matrix
        #:num-utils.utilities
        #:select
        #:let-plus)
  (:export
   #:diagonal-vector
   #:diagonal-matrix
   #:wrapped-matrix
   #:lower-triangular-matrix
   #:upper-triangular-matrix
   #:triangular-matrix
   #:hermitian-matrix
   #:diagonal-matrix-elements
   #:wrapped-matrix-elements
   #:transpose))

(cl:defpackage #:num-utils.matrix-shorthand
  (:nicknames #:nu.mx)
  (:use #:cl
        #:alexandria
        #:anaphora
        #:num-utils.matrix
        #:num-utils.utilities
        #:let-plus)
  (:export
   #:vec
   #:mx
   #:diagonal-mx
   #:lower-triangular-mx
   #:hermitian-mx
   #:upper-triangular-mx))

(cl:defpackage #:num-utils.quadrature
  (:use #:cl
        #:alexandria
        #:anaphora
        #:num-utils.arithmetic
        #:num-utils.interval
        #:num-utils.utilities
        #:let-plus)
  (:export
   #:romberg-quadrature))

(cl:defpackage #:num-utils.rootfinding
  (:use #:cl
        #:alexandria
        #:num-utils.interval
        #:num-utils.utilities
        #:let-plus)
  (:export
   #:*rootfinding-epsilon*
   #:*rootfinding-delta-relative*
   #:root-bisection))

(cl:defpackage #:num-utils.statistics
  (:nicknames #:nu.stats)
  (:use #:cl
        #:anaphora
        #:alexandria
        #:num-utils.arithmetic
        #:num-utils.num=
        #:num-utils.utilities
        #:let-plus)
  (:shadow #:mean
           #:variance
           #:median)
  (:export
   #:tally
   #:add
   #:pool
   #:empty-accumulator
   #:not-enough-elements-in-accumulator
   #:information-not-collected-in-accumulator
   #:central-sample-moments
   #:central-sample-moments-degree
   #:*central-sample-moments-default-degree*
   #:mean
   #:variance
   #:sd
   #:central-m2
   #:central-m3
   #:central-m4
   #:skewness
   #:kurtosis
   #:median
   #:sorted-reals
   #:sorted-reals-elements
   #:empirical-quantile
   #:empirical-quantile-probabilities
   #:quantile
   #:quantiles
   #:ensure-sorted-reals
   #:ensure-sorted-vector
   #:weighted-quantiles
   #:make-sparse-counter
   #:sparse-counter
   #:sparse-counter-count
   #:sparse-counter-table
   #:tabulate
   #:cross-tabulate))

(cl:defpackage #:num-utils.test-utilities
  (:use #:cl)

  (:import-from #:num-utils.num=
		#:num-delta)

  (:import-from #:num-utils.arithmetic
		#:square)

  (:export #:test-results

	   ;; struct accessors
	   #:worst-case ; row at which the worst error occurred
	   #:min-error  ; smallest relative error found
	   #:max-error  ; largest relative error found
	   #:mean-error ; mean error found
	   #:test-count ; number of test cases
	   #:variance0  ; variance of the errors found
	   #:variance1  ; unbiased variance of the errors found
	   #:rms        ; Root Mean Square, or quadratic mean of the error

	   ;; Testing functions
	   #:test-fn
	   #:compare-fns
	   #:compare-vectors))

