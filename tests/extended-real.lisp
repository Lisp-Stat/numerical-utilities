;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite extended-real
    :description "Test extended real functions"
    :in all-tests)
(in-suite extended-real)

;;; helper functions for defining tests

(defun assert-relation (relation &rest argument-lists)
  "Assert RELATION called with each set of arguments."
  (loop for a in argument-lists
        do (is (apply relation a))))

(defun assert-not-relation (relation &rest argument-lists)
  "Assert that RELATION does not hold, called with each set of arguments."
  (loop for a in argument-lists
        do (not (apply relation a))))

(defun assert-paired-relation (relation1 relation2 &rest argument-lists)
  (apply #'assert-relation relation1 argument-lists)
  (apply #'assert-relation relation2 (mapcar #'reverse argument-lists)))

(defun assert-not-paired-relation (relation1 relation2 &rest argument-lists)
  (apply #'assert-not-relation relation1 argument-lists)
  (apply #'assert-not-relation relation2 (mapcar #'reverse argument-lists)))

(defun assert-relation-corner-cases (&rest relations)
  (loop for r in relations
        do (is (funcall r 1))
           (is (funcall r :plusinf))
           (is (funcall r :minusinf))
           (signals error (funcall r))))

(test relation-corner-cases-test
  (assert-relation-corner-cases #'xreal:= #'xreal:< #'xreal:> #'xreal:>= #'xreal:<=))

(test strict-inequalities-test
  (assert-paired-relation #'xreal:< #'xreal:>
                          ;; < pairs
                          '(1 2)
                          '(1 :plusinf)
                          '(:minusinf :plusinf)
                          '(:minusinf 1)
                          ;; < sequences
                          '(1 2 3)
                          '(1 2 :plusinf)
                          '(:minusinf 1 4 :plusinf))
  (assert-not-paired-relation #'xreal:< #'xreal:>
                              ;; not < pairs
                              '(1 1)
                              '(2 1)
                              '(:plusinf :plusinf)
                              '(:plusinf 1)
                              '(:minusinf :minusinf)
                              '(:plusinf :minusinf)
                              '(1 :minusinf)
                              ;; not < sequences
                              '(1 2 2)
                              '(1 3 2)
                              '(1 :plusinf 2)
                              '(1 :plusinf :plusinf)))

(test inequalities-test
  (assert-paired-relation #'xreal:<= #'xreal:>=
                          ;; <= pairs
                          '(1 1)
                          '(1 2)
                          '(1 :plusinf)
                          '(:plusinf :plusinf)
                          '(:minusinf :plusinf)
                          '(:minusinf :minusinf)
                          '(:minusinf 1)
                          ;; < sequences
                          '(1 2 2)
                          '(1 2 3)
                          '(1 2 :plusinf)
                          '(1 :plusinf :plusinf)
                          '(:minusinf 1 4 :plusinf))
  (assert-not-paired-relation #'xreal:<= #'xreal:>=
                              ;; not < pairs
                              '(2 1)
                              '(:plusinf 1)
                              '(:plusinf :minusinf)
                              '(1 :minusinf)
                              ;; not <=/>= sequences
                              '(1 3 2)
                              '(1 :plusinf 2)))

(test equality-test
  (assert-relation #'xreal:=
                   ;; = pairs
                   '(1 1)
                   '(:plusinf :plusinf)
                   '(:minusinf :minusinf)
                   ;; = sequences
                   '(2 2 2)
                   '(:plusinf :plusinf :plusinf)
		   '(:minusinf :minusinf :minusinf))
  (assert-not-relation #'xreal:=
                       ;; not = pairs
                       '(1 2)
                       '(2 1)
                       '(1 :plusinf)
                       '(:plusinf 1)
                       '(1 :minusinf)
                       '(:minusinf 1)
                       ;; not = sequences
                       '(1 2 2)
                       '(2 2 1)
                       '(:plusinf :plusinf 9)
                       '(:plusinf :minusinf)))
