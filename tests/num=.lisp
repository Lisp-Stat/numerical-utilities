;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite num=
    :description "Tests num= functions"
    :in all-tests)
(in-suite num=)

(test num=-number-test
  (let ((*num=-tolerance* 1e-3))
    (is (num= 1 1))
    (is (num= 1 1.0))
    (is (num= 1 1.001))
    (not (num= 1 2))
    (not (num= 1 1.01))))

(test num=-list-test
  (let ((*num=-tolerance* 1e-3))
    (is (num= nil nil))
    (is (num= '(1) '(1.001)))
    (is (num= '(1 2) '(1.001 1.999)))
    (not (num= '(0 1) '(0 1.02)))
    (not (num= nil '(1)))))

(test num=-array-test
  (let* ((*num=-tolerance* 1e-3)
         (a #(0 1 2))
         (b #2A((0 1)
                (2 3))))
    (is (num= a a))
    (is (num= a #(0 1.001 2)))
    (is (num= a #(0 1.001 2.001)))
    (is (num= b b))
    (is (num= b #2A((0 1)
		    (2.001 3))))
    (not (num= a b))
    (not (num= a #(0 1)))
    (not (num= a #(0 1.01 2)))
    (not (num= b #2A((0 1))))
    (not (num= b #2A((0 1.01)
		     (2 3))))))

(defstruct num=-test-struct
  "Structure for testing DEFINE-STRUCTURE-num=."
  a b)

(define-structure-num= num=-test-struct a b)

(test num=-structure-test
  (let ((*num=-tolerance* 1e-3)
        (a (make-num=-test-struct :a 0 :b 1))
        (b (make-num=-test-struct :a "string" :b nil)))
    (is (num= a a))
    (is (num= a (make-num=-test-struct :a 0 :b 1)))
    (is (num= a (make-num=-test-struct :a 0 :b 1.001)))
    (not (num= a (make-num=-test-struct :a 0 :b 1.01)))
    (is (num= b b))
    (not (num= a b))))
