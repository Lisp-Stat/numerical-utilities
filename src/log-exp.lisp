;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.LOG-EXP -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:num-utils.log-exp)

;;; Functions based on log and exp that require special handling near
;;; zero

;;; <http://www.plunk.org/~hatch/rightway.php> for log1+ and exp-1,
;;; both ultimately from a document of Kahan's somewhere on his web
;;; page: http://people.eecs.berkeley.edu/~wkahan/

(declaim (inline log1+ log1-))

(defun log1+ (x)
  "Compute (log (1+ x)) stably even when X is near 0."
  (let ((u (+ 1 x)))
    (if (= u 1)
        x
        (/ (* x (ln u))
           (- u 1)))))

(defun log1- (x)
  "Compute (log (- 1 x)) stably even when X is near zero."
  (log1+ (- x)))

(defun log1+/x (x)
  "Compute (/ (log (+ 1 x)) x) stably even when X is near zero."
  (let ((u (+ x 1)))
    (if (= u 1)
        x
        (/ (log u)
           (- u 1)))))

(defun exp-1 (x)
  "Compute (- (exp x) 1) stably even when X is near 0"
  (let ((u (exp x)))
    (if (= u 1)
        x
        (let ((v (- u 1)))
          (if (= v -1)
              -1
              (/ (* v x)
                 (ln u)))))))

(defun exp-1/x (x)
  "Compute (/ (- (exp x) 1) x) stably even when X is near zero."
  (let ((u (exp x)))
    (if (= u 1)
        x
        (let ((v (- u 1)))
          (if (= v -1)
              (/ -1 x)
              (/ v (log u)))))))

(defun expt-1 (a z)
  "Compute (a^z)-1 stably even when A is close to 1 or Z is close to
zero."
  (or (and (or (< (abs a) 1)
               (< (abs z) 1))
           (let ((p (* (log a) z)))
             (and (< (abs p) 2)
                  (exp-1 p))))
      (- (expt a z) 1)))

(defun log1-exp (a)
  "Compute log(1-exp(x)) stably even when A is near zero.
This is sometimes known as the E_3, the third Einstein function.
See Mächler 2008 for notes on accurate calculation.
https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf"
  (cond ((or (complexp a) (minusp a))
         ;; XXX
         (log (- 1 (exp a))))
        ((<= a 0)                       ;XXX
         #+ () (log1- (- (exp (- a))))
         (log1+ (- (exp a)))
         #+ () (log (- 1 (exp a))))
        ((<= a #.(log 2d0))
         (log (- (exp-1 a))))
        (t
         ;; The paper has -a, but that's wrong.
         (log1+ (- (exp a))))))

(defun log1+exp (a)
  "Accurately compute log(1+exp(x)) even when A is near zero."
  (if (realp a)
      (let ((x (coerce a 'double-float)))
        (cond ((<= x -37)
               (exp x))
              ((<= x 18)
               (log1+ (exp x)))
              ((<= x 33.3)
               (+ x (exp (- x))))
              (t x)))
      (log (+ 1 (exp a)))))

(defun log2-exp (x)
  "Compute log(2-exp(x)) stably even when X is near zero."
  (log1+ (- (exp-1 x))))

(defun logexp-1 (a)
  "Compute log(exp(a)-1) stably even when A is small."
  (if (realp a)
      (let ((x (coerce a 'double-float)))
        (cond ((<= x -37)
               0d0)
              ((<= x 18)
               (log (exp-1 x)))
              ((<= x 33.3)
               (- x (exp (- x))))
              (t x)))
      (log (- (exp a) 1))))

(defun hypot (x y) ;; TODO: move elsewhere?
  "Compute the hypotenuse of X and Y without danger of floating-point
overflow or underflow."
  (setf x (abs x)
        y (abs y))
  (when (< x y)
    (rotatef x y))
  (* x (sqrt (+ 1 (square (/ y x))))))

#|
References:
  https://github.com/ruricolist/floating-point-contractions/blob/master/floating-point-contractions.lisp
Test data:
  https://code.woboq.org/boost/boost/libs/math/test/log1p_expm1_data.ipp.html
|#
