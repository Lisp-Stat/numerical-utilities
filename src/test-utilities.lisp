;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.TEST-UTILITIES -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.
(cl:in-package #:num-utils.test-utilities)

;;; Utilities for testing accuracy of mathmatical functions

;;; TEST-FN         - Compare a function against known reference values. Used in unit tests.
;;; COMPARE-FN      - Compare a function against a reference implementation, e.g. Cephes
;;; COMPARE-VECTORS - Compare two vectors of pre-computed values.

;;; The examples given assume the special-functions test data has been
;;; loaded. TEST-UTILITIES was developed in support of that
;;; library. Note that different versions of the test arrays may have
;;; the same name, for example NEAR-0. Check the package you are using
;;; if you duplicate the examples.


(defstruct (test-results :conc-name)
  "Differences between reference values and computed values"
  (worst-case   0 :type integer)	; row at which the worst error occurred
  (min-error  0d0 :type double-float)	; smallest relative error found
  (max-error  0d0 :type double-float)	; largest relative error found
  (mean-error 0d0 :type double-float)	; mean error found
  (test-count   0 :type integer)	; number of test cases
  (variance0  0d0 :type double-float)	; variance of the errors found
  (variance1  0d0 :type double-float)	; unbiased variance of the errors found
  (rms        0d0 :type double-float))	; Root Mean Square, or quadratic mean of the error



;;; TEST-FUN

;;; This is the most commonly used testing function. It takes a vector
;;; (column of the test data array) of expected values, a function to
;;; be tested, and parameters to the function. Often the parameters to
;;; the function are other columns in the same array, as is the case
;;; with Boost test data.

;;; The input function FN is typically a lambda function, taking a row
;;; index followed by parameters for the function. This makes it easy
;;; to rearrange the parameters to suit the function being tested. For
;;; example incomplete-gamma requires keyword arguments, so you can
;;; provide the keys in the lambda function and the gamma function
;;; paramaterization values from the FN-PARAM-COLUMNS.

#| Examples:
To run the Boost Gamma tests using the test values in the
special-functions tests directory:

(test-fn (select factorials t 1) ; expected values are all rows of the 2nd column (0 based indexing)
#'(lambda (i params)
(specfun:gamma (aref (car params) i))) ; extract the parameter(s) from the param columns
(select factorials t 0))		    ; all rows of the 1st column are the parameters to the lambda function

;;; Same principal, using a different test data set
(test-fn (select near-1 t 1)
#'(lambda (i params)
(specfun:gamma (aref (car params) i)))
(select near-1 t 0))
|#

(defun test-fn (expected-column fn &rest fn-param-columns)
  "Test the differences between expected values and the given function"
  (loop
    with max-delta = 0 and worst-case = 0 and count = (length expected-column)
    for i from 0 to (1- count)
    for delta = (num-delta (aref expected-column i) (funcall fn i fn-param-columns))
    when (> delta max-delta) :do (progn (setf max-delta delta)
					(setf worst-case i))
    minimize delta into min
    maximize delta into max
    sum      delta into sum
    sum      (square delta) into sum-of-delta-squares

    finally (return (make-test-results :worst-case worst-case
				       :min-error  min
				       :max-error  max
				       :mean-error (/ sum count)
				       :test-count count
				       :variance0  (/ (- sum-of-delta-squares
							 (/ (square sum)
							    count))
						      count)
				       :variance1  (/ (- sum-of-delta-squares
							 (/ (square sum)
							    count))
						      (1- count))
				       :rms        (sqrt (/ sum-of-delta-squares count))))))




;;; COMPARE-FUNCTIONS

;;; Useful if you don't have 'golden' test data and want to test a
;;; function against a high quality implementation like R, Boost or
;;; Cephes.

;;; FN-PARAMS contain x values and function parametrization values, if
;;; any. All columns must be the same length

#| Examples:
The example assumes that special-functions test data for erf is loaded

(compare-fns #'(lambda (i params)
(specfun:erf (aref (car params) i)))
#'(lambda (i params)
(cephes:erf (aref (car params) i)))
(select erf-data t 0))

|#

(defun compare-fns (fn-1 fn-2 &rest fn-params)
  "Compare the values returned by two functions"
  (loop
    with max-delta = 0 and worst-case = 0 and count = (length (car fn-params))
    for i from 0 to (1- count)
    for delta = (num-delta (funcall fn-1 i fn-params) (funcall fn-2 i fn-params))
    when (> delta max-delta) :do (progn (setf max-delta delta)
					(setf worst-case i))
      minimize delta into min
    maximize delta into max
    sum      delta into sum
    sum      (square delta) into sum-of-delta-squares

    finally (return (make-test-results :worst-case worst-case
				       :min-error  min
				       :max-error  max
				       :mean-error (/ sum count)
				       :test-count count
				       :variance0   (/ (- sum-of-delta-squares
							 (/ (square sum)
							    count))
						      count)
				       :variance1  (/ (- sum-of-delta-squares
							 (/ (square sum)
							    count))
						      (1- count))
				       :rms        (sqrt (/ sum-of-delta-squares count))))))



;;; COMPARE-VECTORS

;;; REFERENCE-VALUES is a vector containing the 'correct' values of
;;; the computation. These can be obtained from precomputed tables,
;;; values from a production system or with a reference function, such
;;; as Cephes.
;;; COMPUTED-VALUES is the values from the function under test
;;; vectors must be of the same size

(defun compare-vectors (reference-values computed-values)
  "Compare two vectors containing the results of previous computations"
  (assert (= (length reference-values)
	     (length computed-values)))
  (loop
    with max-delta = 0 and worst-case = 0 and count = (length reference-values)
    for i from 0 to (1- count)
    for delta = (num-delta (aref reference-values i)(aref computed-values i))
    when (> delta max-delta) :do (progn (setf max-delta delta)
					(setf worst-case i))
      minimize delta into min
    maximize delta into max
    sum      delta into sum
    sum      (square delta) into sum-of-delta-squares

    finally (return (make-test-results :worst-case worst-case
				       :min-error  min
				       :max-error  max
				       :mean-error (/ sum count)
				       :test-count count
				       :variance0   (/ (- sum-of-delta-squares
							 (/ (square sum)
							    count))
						      count)
				       :variance1  (/ (- sum-of-delta-squares
							 (/ (square sum)
							    count))
						      (1- count))
				       :rms        (sqrt (/ sum-of-delta-squares count))))))


