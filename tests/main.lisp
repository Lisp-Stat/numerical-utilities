;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2021-2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

;;; Root test suite — all sub-suites hang off this.
(defsuite all-tests ())

#+genera (setf *print-array* t)

;;; These two came from special-functions when I moved log-exp here.
(defparameter *report-epsilon* t
  "Print key statistics in terms of machine epsilon.")

(defun print-test-summary (result &key (report-epsilon *report-epsilon*))
  "Print summary of results.
Include some values in epsilon if report-epsilon is true. This is
useful when comparing to other implementations."
  (write result)
  (when report-epsilon
    (format t "~%   Key stats in terms of epsilon:~%     Max = ~,2Eε (Mean = ~,2Eε)~%"
            (/ (max-error  result) double-float-epsilon)
            (/ (mean-error result) double-float-epsilon))))

(defun eps (x)
  "Return a multiple of double-float-epsilon."
  (* x double-float-epsilon))

(defun run-tests (&optional (report-progress t))
  "Run all num-utils tests."
  (let ((*print-pretty* t)
        (*test-output-stream* *standard-output*))
    (run-suite 'all-tests :report-progress report-progress)))

