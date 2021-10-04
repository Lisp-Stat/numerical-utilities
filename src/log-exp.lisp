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

;; Julia, the source of this, has only three basic tests (NaN, +/-
;; infinity). My spot-testing against the R implementation shows exact
;; match to 16 decimals.
(defun log1pmx (x)
  "Compute (- (log (1+ x)) x)
Accuracy within ~2ulps for -0.227 < x < 0.315"
  (declare (double-float x))
  (let+ (((&flet kernel (x)
	    (let* ((r (/ x (+ 2 x)))
		   (s (square r))
		   (w (evaluate-polynomial (coerce #(1.17647058823529412d-1 ;2/17
						     1.33333333333333333d-1 ;2/15
						     1.53846153846153846d-1 ;2/13
						     1.81818181818181818d-1 ;2/11
						     2.22222222222222222d-1 ;2/9
						     2.85714285714285714d-1 ;2/7
						     4d-1		    ;2/5
						     6.66666666666666667d-1);2/3
						   'simple-double-float-vector)
					   s))
		   (hxsq (* 0.5d0 x x)))
	      (- (* r (+ hxsq (* w s))) hxsq)))))
    (cond
      ((not (< -0.7 x 0.9)) (- (log1+ x) x))
      ((> x 0.315) (let ((u (/ (- x 0.5) 1.5)))
		     (- (kernel u) 9.45348918918356180d-2 (* 0.5 u))))
      ((> x -0.227) (kernel x))
      ((> x -0.4) (let ((u (/ (+ x 0.25) 0.75)))
		    (+ (kernel u) -3.76820724517809274d-2 (* 0.25 u))))
      ((> x -0.6) (let ((u (* (+ x 0.5) 2)))
		    (+ (kernel u) -1.93147180559945309d-1 (* 0.5 u))))
      (t (let ((u (/ (+ x 0.625) 0.375)))
	   (+ (kernel u) -3.55829253011726237d-1 (* 0.625 u)))))))

#|
References:
  https://github.com/ruricolist/floating-point-contractions/blob/master/floating-point-contractions.lisp
Test data:
  https://code.woboq.org/boost/boost/libs/math/test/log1p_expm1_data.ipp.html
|#
