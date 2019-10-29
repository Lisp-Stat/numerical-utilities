;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;;; Copyright (c) 2010 by Tamas K. Papp <tkpapp@gmail.com>
;;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:num-utils
  :description "Numerical utilities for Common Lisp"
  :version "0.0.2"
  :author "Steven Nunez <steve.nunez@symbolics.com.sg"
  :license "Boost Software License - Version 1.0"
  :depends-on (#:anaphora
               #:alexandria
               #:array-operations
               #:select
               #:let-plus)
  :in-order-to ((test-op (test-op "num-utils/tests")))
  :pathname "src/"
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "num=")
   (:file "arithmetic")
;;   (:file "arithmetic-type") ; now in src/old/ Looks like it was a WIP
   (:file "elementwise")
   (:file "extended-real")
   (:file "interval")
   (:file "print-matrix")
   (:file "matrix")
   (:file "matrix-shorthand")
   (:file "statistics")
   (:file "chebyshev")
   (:file "polynomial")
   (:file "rootfinding")
   (:file "quadrature")
   (:file "common-package")))

(asdf:defsystem #:num-utils/tests
  :description "Unit tests for NUM-UTILS."
  :author "Steven Nunez <steve.nunez@symbolics.com.sg"
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
   (:file "statistics")
   (:file "utilities"))
  :perform (asdf:test-op (o s)
			 (uiop:symbol-call :fiveam :run!
					   (uiop:find-symbol* :all-tests
							      :num-utils-tests))))
