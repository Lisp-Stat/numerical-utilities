;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS-TESTS -*-
;;; Copyright (c) 2019, 2023 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils-tests)

#+genera (setf *print-array* t)

(def-suite matrix
    :description "Test matrix functions"
    :in all-tests)
(in-suite matrix)

(test wrapped-univariate-operation
  (is (num= (e- (upper-triangular-mx t 2)) (upper-triangular-mx t -2)))
  (is (num= (e/ (upper-triangular-mx t 2)) (upper-triangular-mx t 0.5)))
  (is (num= (e+ (upper-triangular-mx t 2)) (upper-triangular-mx t 2))))

(defun do-matrix-convert-ops (test converts &key (ops (list #'e+ #'e- #'e*)))
  "Funcall TEST with CONVERT and each operation in OPs."
  (mapc (lambda (convert)
          (mapc (curry #'funcall test convert) ops))
        converts))

(defun assert-distributive-convert-op (a b convert op)
  "Check that OP distributes over CONVERT."
  (is (num= (funcall convert (funcall op a b))
	    (funcall op (funcall convert a) (funcall convert b)))))

#| 20191001 (SN) Added during debugging process
(defun assert-associative-convert-op (a b convert op)
  "Check that OP is associative over CONVERT."
  ;; (declare (ignore a b convert op))
  ;; (skip "op a b = op b a. Bug somewhere.")
  ;; (skip "op b a = op a b. Bug somewhere.")
  ;; (let* ((x (funcall op a b))
  ;; 	 (y (funcall op (funcall convert a) b)))
  ;;   (is (num= x y)
  ;; 	"Expected x, ~A, to be equal to y, ~A.~%funcall op a b returns: ~A~%funcall op funcall convert a returns: ~A~%"
  ;; 	x
  ;; 	y
  ;; 	(funcall op a b)
  ;; 	(funcall op (funcall convert a) b)
  ;; 	)))
  (format t "~%num= returns ~A~%" (num= (funcall op a b)
  					(funcall op (funcall convert a) b)))
  (is (num= (funcall op a b)
  	    (funcall op (funcall convert a) b));)
      "Expected ~A to be equal to ~A" (funcall op a b) (funcall op (funcall convert a) b))
  (format t "~%num= returns ~A~%" (num= (funcall op a b)
  					(funcall op (funcall convert a) b)))
  (is (num= (funcall op a b)
  	    (funcall op a (funcall convert b)))
      "Expected ~A to be equal to ~A" (funcall op a b) (funcall op a (funcall convert b))))
|#

(test wrapped-bivariate-operation
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
#+ignore
(test wrapped-bivariate-to-array
  (let+ ((a (mx t
              (1 2)
              (3 4)))
         (b (mx t
              (5 7)
              (11 13))))
    (do-matrix-convert-ops (curry #'assert-associative-convert-op a b)
      (list #'hermitian-matrix
            #'lower-triangular-matrix
            #'upper-triangular-matrix))))

;;; Transliteration of clunit deftest
;;; Commented out reason-args because they make the failure appear to be a false positive
(test wrapped-bivariate-to-array
  (let+ ((a (mx t
              (1 2)
              (3 4)))
         (b (mx t
              (5 7)
              (11 13))))
    (do-matrix-convert-ops (lambda (convert op)
                             (is (num= (funcall op a b)
			     	       (funcall op (funcall convert a) b)))
			      	 ;; "Expected ~A but received ~A" (funcall op a b) (funcall op (funcall convert a) b))
                             (is (num= (funcall op a b)
				       (funcall op a (funcall convert b)))))
				 ;; "Expected ~A but received ~A" (funcall op a b) (funcall op a (funcall convert b))))
      (list #'hermitian-matrix
            #'lower-triangular-matrix
            #'upper-triangular-matrix))))

#| Original clunit test from Papp. Works in clunit
(deftest wrapped-bivariate-to-array (matrix-suite)
  (let+ ((a (mx t
              (1 2)
              (3 4)))
         (b (mx t
              (5 7)
              (11 13))))
    (do-matrix-convert-ops (lambda (convert op)
                             (assert-equality #'num= (funcall op a b)
                                 (funcall op (funcall convert a) b))
                             (assert-equality #'num= (funcall op a b)
                                 (funcall op a (funcall convert b))))
      (list #'hermitian-matrix
            #'lower-triangular-matrix
            #'upper-triangular-matrix))))
|#
(test diagonal-test
  (do-matrix-convert-ops (curry #'assert-distributive-convert-op
                                (vec t 1 2 3 4)
                                (vec t 5 7 11 13))
    (list #'diagonal-matrix)))

(test wrapped-matrix-slice
  (let+ ((mx (mx t
               (1 2 3)
               (4 5 6)
               (7 8 9)))
         ((&macrolet assert-slice (type)
            (check-type type symbol)
            `(let* ((wrapped (,type mx))
                    (slice (range 0 2))
                    (sliced (select wrapped slice)))
               (is (eq ',type (type-of sliced)))
               (is (num= sliced (,type (select mx slice slice))))))))
    (assert-slice upper-triangular-matrix)
    (assert-slice lower-triangular-matrix)
    (assert-slice hermitian-matrix)))



