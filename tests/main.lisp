;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

(def-suite all-tests
    :description "The master suite of all NUMERIC-UTILITIES tests")

(in-suite all-tests)

#+genera (setf *print-array* t)

;;; These two came from special-functions when I moved log-exp
;;; here. Might be worth consolidating at some point.

(defparameter *report-epsilon* t "Print key statistics in terms of machine epsilon")

(defun print-test-summary (result &key (report-epsilon *report-epsilon*))
  "Print summary of results.
Include some values in epsilon if report-epsilon is true. This is useful when comparing to other implementations"
  (write result)
  (when report-epsilon
    (format t "~%   Key stats in terms of epsilon:~%     Max = ~,2Eε (Mean = ~,2Eε)~%"
	    (/ (max-error  result) double-float-epsilon)
	    (/ (mean-error result) double-float-epsilon))))

(defun eps (x)
  "Return a multiple of epsilon"
  (* x double-float-epsilon))

(defun test-nu ()
  (run! 'all-tests))

