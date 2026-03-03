;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019, 2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(defsuite matrix (all-tests))

(deftest wrapped-univariate-operation (matrix)
  "Test element-wise unary operations on wrapped matrices."
  (assert-true (num= (e- (upper-triangular-mx t 2)) (upper-triangular-mx t -2)))
  (assert-true (num= (e/ (upper-triangular-mx t 2)) (upper-triangular-mx t 0.5)))
  (assert-true (num= (e+ (upper-triangular-mx t 2)) (upper-triangular-mx t 2))))

(defun do-matrix-convert-ops (test converts &key (ops (list #'e+ #'e- #'e*)))
  "Funcall TEST with CONVERT and each operation in OPs."
  (mapc (lambda (convert)
          (mapc (curry #'funcall test convert) ops))
        converts))

(defun assert-distributive-convert-op (a b convert op)
  "Check that OP distributes over CONVERT."
  (assert-true (num= (funcall convert (funcall op a b))
                     (funcall op (funcall convert a) (funcall convert b)))))

(deftest wrapped-bivariate-operation (matrix)
  "Test distributivity of bivariate element-wise operations over matrix wrappers."
  (do-matrix-convert-ops (curry #'assert-distributive-convert-op
                                (mx t
                                  (1 2)
                                  (3 4))
                                (mx t
                                  (5 7)
                                  (11 13)))
    (list #'hermitian-matrix
          #'lower-triangular-matrix
          #'upper-triangular-matrix)))

;;; Original clunit test from Papp -- now active as deftest.
(deftest wrapped-bivariate-to-array (matrix)
  "Test that wrapped operations on arrays give same result."
  (let+ ((a (mx t
              (1 2)
              (3 4)))
         (b (mx t
              (5 7)
              (11 13))))
    (do-matrix-convert-ops (lambda (convert op)
                             (assert-true (num= (funcall op a b)
                                               (funcall op (funcall convert a) b)))
                             (assert-true (num= (funcall op a b)
                                               (funcall op a (funcall convert b)))))
      (list #'hermitian-matrix
            #'lower-triangular-matrix
            #'upper-triangular-matrix))))

(deftest diagonal-test (matrix)
  "Test diagonal matrix operations."
  (do-matrix-convert-ops (curry #'assert-distributive-convert-op
                                (vec t 1 2 3 4)
                                (vec t 5 7 11 13))
    (list #'diagonal-matrix)))

(deftest wrapped-matrix-slice (matrix)
  "Test that slicing a wrapped matrix returns the correct wrapped type."
  (let+ ((mx (mx t
               (1 2 3)
               (4 5 6)
               (7 8 9)))
         ((&macrolet assert-slice (type)
            (check-type type symbol)
            `(let* ((wrapped (,type mx))
                    (slice (range 0 2))
                    (sliced (select wrapped slice)))
               (assert-true (eq ',type (type-of sliced)))
               (assert-true (num= sliced (,type (select mx slice slice))))))))
    (assert-slice upper-triangular-matrix)
    (assert-slice lower-triangular-matrix)
    (assert-slice hermitian-matrix)))
