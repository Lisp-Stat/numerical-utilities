;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2010 by Tamas K. Papp <tkpapp@gmail.com>
;;; Copyright (c) 2019-2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(defsystem "num-utils"
  :version "1.5.0"
  :license :MS-PL
  :author "Steven Nunez <steve@symbolics.tech>"
  :long-name "Numerical Utilities"
  :description "Numerical utilities for Common Lisp"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  ;:homepage    "https://lisp-stat.dev/docs/tasks/plotting/"
  :source-control (:git "https://github.com/Lisp-Stat/numerical-utilities.git")
  :bug-tracker "https://github.com/Lisp-Stat/numerical-utilities/issues"
  :depends-on (#:anaphora
               #:alexandria
	       #:alexandria+
               #:array-operations
               #:select
               #:let-plus)
  :in-order-to ((test-op (test-op "num-utils/tests")))
  :pathname "src/"
  :serial t
  :components ((:file "utilities")
               (:file "arithmetic")
               (:file "num=")
               (:file "extended-real")
               (:file "interval")
               (:file "chebyshev")
               (:file "polynomial")
               (:file "elementwise")
               (:file "print-matrix")
               (:file "matrix")
               (:file "matrix-shorthand")
               (:file "quadrature")
	       (:file "norms")
               (:file "rootfinding")
               (:file "log-exp")
               (:file "test-utilities")
               (:file "pkgdcl")))

(defsystem "num-utils/tests"
  :version "1.0.0"
  :description "Unit tests for NUM-UTILS."
  :author "Steven Nunez <steve@symbolics.tech>"
  :license "Same as NUM-UTILS -- this is part of the NUM-UTILS library."
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:num-utils
               #:fiveam
	       #:select) ; matrix test needs this
  :pathname "tests/"
  :serial t
  :components
  ((:file "test-package")
   (:file "main")
   ;; in alphabetical order
   (:file "arithmetic")
;; (:file "arithmetic-type") ; No tests included in Papp's version
   (:file "chebyshev")
   (:file "polynomial")
   (:file "elementwise")
   (:file "extended-real")
   (:file "interval")
   (:file "matrix")
   (:file "matrix-shorthand")
   (:file "num=")
   (:file "quadrature")
   (:file "rootfinding")
   (:file "log-exp")
   (:file "norms")
   (:file "utilities"))
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam :run!
				      (uiop:find-symbol* :all-tests
							 :num-utils-tests))))
