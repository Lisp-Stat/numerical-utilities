;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
(in-package "NUM-UTILS-TESTS")

(def-suite all-tests
    :description "The master suite of all NUMERIC-UTILITIES tests")

(in-suite all-tests)

#+genera (setf *print-array* t)

(defun test-nu ()
  (run! 'all-tests))

