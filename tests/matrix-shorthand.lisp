;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite matrix-shorthand
    :description "Tests matrix-shorthand functions"
    :in all-tests)
(in-suite matrix-shorthand)

(test lower-triangular-shorthand-test
  (let ((matrix #2A((1 2)
                    (3 4)))
        (lower-triangular-mx (lower-triangular-mx t
                               (1)
                               (3 4))))
    (is (num= (lower-triangular-matrix matrix) lower-triangular-mx))
    (is (num= lower-triangular-mx (lower-triangular-mx t
                                                  (1 9) ; 9 should be ignored
                                                  (3 4))))
    (is (num= (lower-triangular-mx t
                              (1 2 3)
                              (3 4 5))
        (lower-triangular-mx t
          (1 2 17)
          (3 4 5))))
    (is (num= (lower-triangular-mx t
                              (1 2)
                              (3 4)
                              (5 6))
        (lower-triangular-mx t
          (1 19)
          (3 4)
          (5 6))))))

(test upper-triangular-shorthand-test
  (let ((matrix #2A((1 2)
                    (3 4)))
        (upper-triangular-mx (upper-triangular-mx t
                   (1 2)
                   (3 4))))
    (is (num= (upper-triangular-matrix matrix) upper-triangular-mx))
    (is (num= upper-triangular-mx (upper-triangular-mx t
                                      (1 2)
                                      (9 4)))) ; 9 should be ignored
    (is (num= (upper-triangular-mx t
                              (1 2 3)
                              (3 4 5))
        (upper-triangular-mx t
          (1 2 3)
          (19 4 5))))
    (is (num= (upper-triangular-mx t
                              (1 2)
                              (3 4)
                              (5 6))
        (upper-triangular-mx t
          (1 2)
          (3 4)
          (19 6))))))

(test hermitian-shorthand-test
  (let ((matrix #2A((1 2)
                    (3 4)))
        (hermitian-mx (hermitian-mx t
                        (1)
                        (3 4))))
    (is (num= hermitian-mx (hermitian-matrix matrix)))
    (is (num= hermitian-mx (hermitian-mx t
                                           (1 9) ; 9 should be ignored
                                           (3 4))))
    (signals error (hermitian-mx t
                              (1 2 3)
                              (3 4 5)))))

(test diagonal-shorthand-test
  (is (num= (diagonal-mx t 1 2 3) (diagonal-matrix #(1 2 3)))))

(test vec-shorthand-test
  (is (num= (vec t 1 2 3) #(1 2 3))))
