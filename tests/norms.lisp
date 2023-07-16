;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite norms
    :description "L-norm functions"
    :in all-tests)
(in-suite norms)

(defparameter data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
(defparameter zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                      #C(-2 3) #C(-3 1) #C(-1 0)))

(defparameter arr #2A((1.1 1.2 1.3 1.4)
		      (2.1 2.2 2.3 2.4)
		      (3.1 3.2 3.3 3.4)
		      (4.1 4.2 4.3 4.4)
		      (5.1 5.2 5.3 5.4)))

(defparameter a #(-4 -3 -2 -1  0  1  2  3  4))
(defparameter b (aops:reshape a '(3 3)))



(test norms
  (let* ((a #(2 3 4))
         (a-list (coerce a 'list))
         (b #(#C(3 4) 0 5 5 5))
         (b-list (coerce b 'list)))
    (is (num= (sqrt 29) (l2norm a)))
    (is (num= (sqrt 29) (l2norm a-list)))
    (is (num= 10 (l2norm b)))
    (is (num= 10 (l2norm b-list)))))


;;; These tests came from the linear-algabra system


;;; Taxicab norm

(test unary-norm-1-vector
  (is (num= 36 (norm data 1)))
  (is (num= 19.535658 (norm zdata 1))))

;;; Euclidean norm

(test unary-norm-2-vector
  (is (num= 12.083046 (norm data 2)))
  (is (num= 8.0 (norm zdata 2))))

;;; P-norm

(test unary-norm-p-vector
    (is (num= 8.732892 (norm data 3)))
    (is (num= 6.064035 (norm zdata 3))))

;;; Infinity norm

(test unary-norm-infinity-vector
  (is (num= 6   (norm data :inf)))
  (is (num= 4.0 (norm zdata :inf))))

(test unary-norm-array
  (is (num= 17.0 (norm arr 1)))
  ;; (is (num= 5.4 (norm arr :max)))
  (is (num= 15.858751 (norm arr :frob)))
  (is (num= 21.0 (norm arr :inf))))

;; See https://docs.scipy.org/doc/scipy/reference/generated/scipy.linalg.norm.html#scipy.linalg.norm
(test scipy-examples
  (is (num= 7.745966692414834 (norm a)))
  (is (num= 7.745966692414834 (norm b :frob)))
  (is (num= 7.745966692414834 (norm b)))
  (is (num= 4 (norm a :inf)))
  (is (num= 9 (norm b :inf)))
  (is (num= 0 (norm a :-inf)))
  (is (num= 2 (norm b :-inf)))

  (is (num= 20 (norm a 1)))
  (is (num= 7  (norm b 1)))
  (is (num= 7.745966692414834 (norm a 2)))
  (is (num= 7.745966692414834 (norm a 2))))
