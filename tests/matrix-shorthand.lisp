;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019, 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(defsuite matrix-shorthand (all-tests))

(deftest lower-triangular-shorthand-test (matrix-shorthand)
  "Test lower-triangular-mx shorthand ignores upper-triangle elements."
  (let ((matrix #2A((1 2)
                    (3 4)))
        (lower-triangular-mx (lower-triangular-mx t
                               (1)
                               (3 4))))
    (assert-true (num= (lower-triangular-matrix matrix) lower-triangular-mx))
    (assert-true (num= lower-triangular-mx (lower-triangular-mx t
                                                                 (1 9)  ; 9 ignored
                                                                 (3 4))))
    (assert-true (num= (lower-triangular-mx t
                                            (1 2 3)
                                            (3 4 5))
                       (lower-triangular-mx t
                                            (1 2 17)  ; 17 ignored (upper triangle)
                                            (3 4 5))))
    (assert-true (num= (lower-triangular-mx t
                                            (1 2)
                                            (3 4)
                                            (5 6))
                       (lower-triangular-mx t
                                            (1 19)  ; 19 ignored
                                            (3 4)
                                            (5 6))))))

(deftest upper-triangular-shorthand-test (matrix-shorthand)
  "Test upper-triangular-mx shorthand ignores lower-triangle elements."
  (let ((matrix #2A((1 2)
                    (3 4)))
        (upper-triangular-mx (upper-triangular-mx t
                                                  (1 2)
                                                  (3 4))))
    (assert-true (num= (upper-triangular-matrix matrix) upper-triangular-mx))
    (assert-true (num= upper-triangular-mx (upper-triangular-mx t
                                                                 (1 2)
                                                                 (9 4)))) ; 9 ignored
    (assert-true (num= (upper-triangular-mx t
                                            (1 2 3)
                                            (3 4 5))
                       (upper-triangular-mx t
                                            (1 2 3)
                                            (19 4 5))))  ; 19 ignored
    (assert-true (num= (upper-triangular-mx t
                                            (1 2)
                                            (3 4)
                                            (5 6))
                       (upper-triangular-mx t
                                            (1 2)
                                            (3 4)
                                            (19 6))))))  ; 19 ignored

(deftest hermitian-shorthand-test (matrix-shorthand)
  "Test hermitian-mx shorthand and error on non-square spec."
  (let ((matrix #2A((1 2)
                    (3 4)))
        (hermitian-mx (hermitian-mx t
                                    (1)
                                    (3 4))))
    (assert-true (num= hermitian-mx (hermitian-matrix matrix)))
    (assert-true (num= hermitian-mx (hermitian-mx t
                                                  (1 9)  ; 9 ignored
                                                  (3 4))))
    (assert-condition error (hermitian-mx t
                                          (1 2 3)
                                          (3 4 5)))))

(deftest diagonal-shorthand-test (matrix-shorthand)
  "Test diagonal-mx shorthand."
  (assert-true (num= (diagonal-mx t 1 2 3) (diagonal-matrix #(1 2 3)))))

(deftest vec-shorthand-test (matrix-shorthand)
  "Test vec shorthand."
  (assert-true (num= (vec t 1 2 3) #(1 2 3))))
