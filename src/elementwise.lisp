;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.ELEMENTWISE -*-
;;; Copyright (c) 2011-2014 Tamas Papp
;;; Copyright (c) 2023, 2025 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:num-utils.elementwise
  (:use #:cl
        #:alexandria
        #:num-utils.arithmetic
        #:num-utils.utilities
        #:let-plus)
  (:nicknames #:elmt)			;num-util elementwise mathematics
  (:export
   ;; Infrastructure
   #:elementwise-float-contagion
   ;; Variadic wrappers
   #:e+
   #:e-
   #:e*
   #:e/
   #:elog
   #:eatan
   ;; Unary (e1) operators
   #:e1-
   #:e1/
   #:e1log
   #:e1atan
   ;; Unary with direct names
   #:eabs
   #:eexp
   #:esqrt
   #:efloor
   #:eceiling
   #:econjugate
   #:esquare
   #:esin
   #:ecos
   #:etan
   #:easin
   #:eacos
   #:esinh
   #:ecosh
   #:etanh
   #:easinh
   #:eacosh
   #:eatanh
   #:e1round
   #:e1truncate
   #:esignum
   ;; Tier 2 unary — complex accessors
   #:ephase
   #:erealpart
   #:eimagpart
   #:ecis
   ;; Tier 2 unary — float rounding
   #:e1ffloor
   #:e1fceiling
   #:e1fround
   #:e1ftruncate
   ;; Binary (e2) operators
   #:e2+
   #:e2-
   #:e2*
   #:e2/
   #:e2log
   #:eexpt
   #:emod
   #:e2mod
   #:e2<
   #:e2<=
   #:e2>
   #:e2>=
   #:e2=
   #:e2/=
   #:e2atan
   #:e2round
   #:e2truncate
   #:e2floor
   #:e2ceiling
   #:erem
   ;; Tier 2 binary — complex construction
   #:ecomplex
   ;; Tier 2 binary — float rounding
   #:e2ffloor
   #:e2fceiling
   #:e2fround
   #:e2ftruncate
   ;; Tier 2 binary — max/min
   #:e2max
   #:e2min
   ;; Reductions
   #:ereduce
   #:emin
   #:emax)
  (:documentation "Provides elementwise operations for arrays and numbers with automatic type contagion. Supports unary operations (abs, sqrt, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, floor, ceiling, round, truncate, signum, conjugate, phase, realpart, imagpart, cis, ffloor, fceiling, fround, ftruncate), binary operations (+, -, *, /, expt, log, mod, rem, atan, floor, ceiling, round, truncate, complex, ffloor, fceiling, fround, ftruncate, max, min, comparisons), and variadic operators (+, -, *, /, log, atan). Includes reduction functions (min, max, reduce) and seamlessly handles scalar-array and array-array operations."))
(in-package #:num-utils.elementwise)

(defun elementwise-float-contagion (&rest objects)
  "Return the resulting float type when objects (or their elements) are combined using arithmetic operations."
  ;; TODO benchmark, optimize
  (let* ((matrix (load-time-value
                  (let ((matrix (make-array `(10 10)
                                            :element-type '(integer 0 9))))
                    (dotimes (i1 10)
                      (dotimes (i2 10)
                        (let+ (((&values c1 f1) (floor i1 5))
                               ((&values c2 f2) (floor i2 5)))
                          (setf (aref matrix i1 i2)
                                (+ (max f1 f2) (* 5 (max c1 c2)))))))
                    matrix))))
    (declare (type (simple-array (integer 0 9) (10 10)) matrix))
    (if objects
        (aref #(real
                short-float
                single-float
                double-float
                long-float
                complex
                (complex short-float)
                (complex single-float)
                (complex double-float)
                (complex long-float))
              (reduce (lambda (i1 i2) (aref matrix i1 i2)) objects
                      :key (lambda (object)
                             (cond
                               ((arrayp object)
                                (let ((type (array-element-type object)))
                                  (cond
                                    ((subtypep type 'short-float) 1)
                                    ((subtypep type 'single-float) 2)
                                    ((subtypep type 'double-float) 3)
                                    ((subtypep type 'long-float) 4)
                                    ((subtypep type 'real) 0)
                                    ((subtypep type '(complex short-float)) 6)
                                    ((subtypep type '(complex single-float)) 7)
                                    ((subtypep type '(complex double-float)) 8)
                                    ((subtypep type '(complex long-float)) 9)
                                    ((subtypep type 'complex) 5)
                                    (t (return-from elementwise-float-contagion t)))))
                               ((typep object 'short-float) 1)
                               ((typep object 'single-float) 2)
                               ((typep object 'double-float) 3)
                               ((typep object 'long-float) 4)
                               ((typep object 'real) 0)
                               ((typep object '(complex short-float)) 6)
                               ((typep object '(complex single-float)) 7)
                               ((typep object '(complex double-float)) 8)
                               ((typep object '(complex long-float)) 9)
                               ((typep object 'complex) 5)
                               (t (return-from elementwise-float-contagion t))))))
        t)))

;;; various elementwise operations

(defmacro mapping-array ((ref array &rest other) form)
  (check-type ref symbol)
  (with-unique-names (result index)
    (once-only (array)
      `(let ((,result (make-array (array-dimensions ,array)
                                  :element-type (elementwise-float-contagion
                                                 ,array ,@other))))
         (dotimes (,index (array-total-size ,result))
           (setf (row-major-aref ,result ,index)
                 (flet ((,ref (array)
                          (row-major-aref array ,index)))
                   ,form)))
         ,result))))

(defmacro define-e1 (operation
                     &key (function (symbolicate '#:e1 operation))
                          (docstring (format nil "Univariate elementwise ~A."
                                             operation)))
  "Define an univariate elementwise operation."
  (check-types (function operation) symbol)
  `(defgeneric ,function (a)
     (declare (optimize speed))
     (:documentation ,docstring)
     (:method ((a number))
       (,operation a))
     (:method ((a array))
       (mapping-array (m a) (,operation (m a))))))

(define-e1 -)
(define-e1 /)
(define-e1 log)
(define-e1 abs       :function eabs)
(define-e1 floor     :function efloor)
(define-e1 ceiling   :function eceiling)
(define-e1 exp       :function eexp)
(define-e1 sqrt      :function esqrt)
(define-e1 conjugate :function econjugate)
(define-e1 square    :function esquare)
(define-e1 sin       :function esin)
(define-e1 cos       :function ecos)

;;; Tier 1 — trigonometric family
(define-e1 tan       :function etan)
(define-e1 asin      :function easin)
(define-e1 acos      :function eacos)
(define-e1 atan      :function e1atan
  :docstring "Univariate elementwise atan (single-argument arctangent).")
(define-e1 sinh      :function esinh)
(define-e1 cosh      :function ecosh)
(define-e1 tanh      :function etanh)
(define-e1 asinh     :function easinh)
(define-e1 acosh     :function eacosh)
(define-e1 atanh     :function eatanh)

;;; Tier 1 — rounding and misc
(define-e1 round     :function e1round)
(define-e1 truncate  :function e1truncate)
(define-e1 signum    :function esignum)

;;; Tier 2 — complex accessors
(define-e1 phase     :function ephase)
(define-e1 realpart  :function erealpart)
(define-e1 imagpart  :function eimagpart)
(define-e1 cis       :function ecis)

;;; Tier 2 — float rounding (unary)
(define-e1 ffloor    :function e1ffloor)
(define-e1 fceiling  :function e1fceiling)
(define-e1 fround    :function e1fround)
(define-e1 ftruncate :function e1ftruncate)


(defmacro define-e2 (operation
                     &key (function (symbolicate '#:e2 operation))
                          (docstring (format nil "Bivariate elementwise ~A."
                                      operation)))
  "Define a bivariate elementwise operation."
  (check-types (function operation) symbol)
  `(defgeneric ,function (a b)
     (declare (optimize speed))
     (:documentation ,docstring)
     (:method ((a number) (b number))
       (,operation a b))

     ;; Vector class hierarchy. Includes specialised SBCL vectors.
     ;; TODO: See if neccessary. Added during debugging, but this was not the problem.
     (:method ((a vector) (b number))
       (mapping-array (m a b) (,operation (m a) b)))
     (:method ((a number) (b vector))
       (mapping-array (m b a) (,operation a (m b))))
     (:method ((a vector) (b vector))
       (assert (equal (array-dimensions a) (array-dimensions b)))
       (mapping-array (m a b) (,operation (m a) (m b))))

     ;; Array class hierarchy
     (:method ((a array) (b number))
       (mapping-array (m a b) (,operation (m a) b)))
     (:method ((a number) (b array))
       (mapping-array (m b a) (,operation a (m b))))
     (:method ((a array) (b array))
       (assert (equal (array-dimensions a) (array-dimensions b)))
       (mapping-array (m a b) (,operation (m a) (m b))))))


(define-e2 +)
(define-e2 -)
(define-e2 *)
(define-e2 /)
(define-e2 expt :function eexpt)
(define-e2 log)
(define-e2 mod :function emod)
(define-e2 <)
(define-e2 <=)
(define-e2 >)
(define-e2 >=)
(define-e2 =)

;;; Tier 1 — additional binary ops
(define-e2 /=)
(define-e2 atan)
(define-e2 round)
(define-e2 truncate)
(define-e2 floor)
(define-e2 ceiling)
(define-e2 rem :function erem)

;;; Tier 2 — complex construction
(define-e2 complex :function ecomplex)

;;; Tier 2 — float rounding (binary)
(define-e2 ffloor)
(define-e2 fceiling)
(define-e2 fround)
(define-e2 ftruncate)

;;; Tier 2 — binary max/min
(define-e2 max)
(define-e2 min)


(defun elog (a &optional (base nil base?))
  "Elementwise logarithm."
  (if base?
      (e2log a base)
      (e1log a)))

(defun eatan (a &optional (b nil b?))
  "Elementwise arctangent.  With one argument, returns atan(a).  With two arguments, returns atan(a, b) (i.e. atan2)."
  (if b?
      (e2atan a b)
      (e1atan a)))

(defmacro define-e& (operation &key (function (symbolicate '#:e operation))
                                    (bivariate (symbolicate '#:e2 operation))
                                    (univariate (symbolicate '#:e1 operation))
                                    (docstring (format nil "Elementwise ~A."
                                                operation)))
  `(defun ,function (argument &rest more-arguments)
     ,docstring
     (if more-arguments
         (reduce #',bivariate more-arguments :initial-value argument)
         (,univariate argument))))

(define-e& + :univariate identity)
(define-e& -)
(define-e& * :univariate identity)
(define-e& /)

(defgeneric ereduce (function object &key key)
  (:documentation "Elementwise reduce, traversing in row-major order.")
  (:method (function (array array) &key key)
    (reduce function (aops:flatten array) :key key))
  (:method (function (sequence sequence) &key key)
    (reduce function sequence :key key))
  (:method (function object &key key)
    (reduce function (aops:as-array object) :key key)))

(defmacro define-elementwise-reduction
    (name function
     &optional (docstring (format nil "Elementwise ~A." function)))
  `(defun ,name (object)
     ,docstring
     (ereduce #',function object)))

(define-elementwise-reduction emax max)
(define-elementwise-reduction emin min)
