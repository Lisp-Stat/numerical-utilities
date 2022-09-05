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
   #:within?
   #:fixnum?
   #:simple-fixnum-vector
   #:simple-single-float-vector
   #:as-simple-fixnum-vector
   #:simple-boolean-vector
   #:as-bit-vector
   #:as-double-float
   #:with-double-floats
   #:simple-double-float-vector
   #:make-vector
   #:generate-sequence
   #:expanding
   #:bic
   #:binary-search
   #:sequencep				;remove and use alexandria?
   #:as-alist
   #:as-plist)
  (:documentation "A collection of utilities to work with floating point values. Optimised for double-float."))

(defpackage #:num-utils.arithmetic
  (:use #:cl
        #:alexandria-2
	#:alexandria+
        #:anaphora
        #:num-utils.utilities
        #:let-plus)
  (:export
   #:same-sign-p
   #:square
   #:cube
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
  (:export #:evaluate-polynomial #:evaluate-rational)
  (:documentation "Efficient evaluation of polynomial functions using Horner's method"))

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
   #:e2mod
   #:e1log
   #:e1exp
   #:eexpt
   #:eexp
   #:elog
   #:emod
   #:esqrt
   #:efloor
   #:eceiling
   #:econjugate
   #:ereduce
   #:emin
   #:emax
   #:esin
   #:ecos
   #:e2<
   #:e2<=
   #:e2>
   #:e2>=
   #:e2=))

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
   #:transpose
   #:map-array))

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
	#:alexandria+
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

(uiop:define-package #:num-utils.log-exp
    (:use #:cl #:let-plus)

  (:import-from #:num-utils.arithmetic
		#:ln
		#:square)
  (:import-from #:num-utils.polynomial
		#:evaluate-polynomial)
  (:import-from #:num-utils.utilities
		#:simple-double-float-vector)

  (:export #:log1+
	   #:log1-
	   #:log1+/x
	   #:exp-1
	   #:exp-1/x
	   #:expt-1
	   #:log1-exp
	   #:log1+exp
	   #:log2-exp
	   #:logexp-1
	   #:hypot
	   #:log1pmx))

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

