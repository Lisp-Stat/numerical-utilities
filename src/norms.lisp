;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.ELEMENTWISE -*-
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; This is a lisp implemention of the norm functions found in scipy.linalg.

;;; Note that scipy calls out to BLAS and LAPACK for their
;;; implementations and therefore no test suites exist in the scipy
;;; repo.

(uiop:define-package #:num-utils.norms
  (:use #:cl
        #:alexandria-2
	#:alexandria+
        #:let-plus)
  (:import-from #:num-utils.arithmetic #:sum #:seq-min #:seq-max #:absolute-square)
  (:import-from #:num-utils.elementwise #:eexpt #:eabs)
  (:import-from #:num-utils.matrix #:matrixp)
  (:export #:norm
	   #:l2norm-square
	   #:l2norm))
(in-package #:num-utils.norms)


(defgeneric l2norm-square (object)
  (:documentation "Square of the $L_2$ norm of OBJECT.")
  (:method ((sequence sequence))
    (sum sequence :key #'absolute-square)))

(defun l2norm (object)
  "$L_2$ norm of OBJECT."
  (sqrt (l2norm-square object)))

(defun row-sums (m)
  "Return a list containing the sum of the absolute value of each row."
  (loop
    for row below (array-dimension m 0)
    collect (loop
		for col below (array-dimension m 1)
		sum (aref m row col))))

(defun col-sums (m)
  "Return a list containing the sum of each column."
  (loop
    for col below (array-dimension m 1)
    collect (loop
	      for row below (array-dimension m 0)
	      sum (aref m row col))))

;; TODO handle cases where ord < 0
(defun norm (data &optional (ord 2))
  "Matrix or vector norm.

This function is returns one of eight different matrix norms, or one of an infinite number of vector norms (described below), depending on the value of the ORD parameter, where ORD >= 0.

 The following norms can be calculated:

    =====  ============================  ==========================
    ord    norm for matrices             norm for vectors
    =====  ============================  ==========================
    None   Frobenius norm                2-norm
    'fro'  Frobenius norm                --
    'nuc'  nuclear norm                  --
    inf    max(sum(abs(a), axis=1))      max(abs(a))
    -inf   min(sum(abs(a), axis=1))      min(abs(a))
    0      --                            sum(a != 0)
    1      max(sum(abs(a), axis=0))      as below
    -1     min(sum(abs(a), axis=0))      as below
    2      2-norm (largest sing. value)  as below
    -2     smallest singular value       as below
    other  --                            sum(abs(a)**ord)**(1./ord)
    =====  ============================  ==========================

    The Frobenius norm is given by [1]_:

        :math:`||A||_F = [\\sum_{i,j} abs(a_{i,j})^2]^{1/2}`

    The nuclear norm is the sum of the singular values.

    Both the Frobenius and nuclear norm orders are only defined for matrices.

    References
    ----------
    .. [1] G. H. Golub and C. F. Van Loan, *Matrix Computations*, Baltimore, MD, Johns Hopkins University Press, 1985, pg. 15
"
  (check-type data array)
  (if (vectorp data)
      (case ord
	(:inf  (seq-max (eabs data)))
	(:-inf (seq-min (eabs data)))
	(:frob (error "Frobenius norm not defined for vectors"))
	(:nuc  (error "Nuclear norm not defined for vectors"))
	(0 (sum data))	;error checking?
	(t (expt (sum (eexpt (eabs data) ord)) (/ ord))))
      (case ord
	(:nuc  (sum (flatten data)))
	(:frob (expt (sum (eexpt (eabs (aops:flatten data)) 2)) (/ 2)))
	(:inf  (apply #'max (row-sums (eabs data))))
	(:-inf (apply #'min (row-sums (eabs data))))
	(1     (apply #'max (col-sums (eabs data))))
	(-1    (apply #'min (col-sums (eabs data))))
	(0     (error "0 norm not defined for matrices"))
	(t     (expt (sum (eexpt (eabs (aops:flatten data)) ord)) (/ ord))))))



