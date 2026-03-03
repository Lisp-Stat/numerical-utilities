
(defparameter data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
(defparameter zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                      #C(-2 3) #C(-3 1) #C(-1 0)))

(defparameter arr #2A((1.1 1.2 1.3 1.4)
		      (2.1 2.2 2.3 2.4)
		      (3.1 3.2 3.3 3.4)
		      (4.1 4.2 4.3 4.4)
		      (5.1 5.2 5.3 5.4)))

(defparameter a #(-4 -3 -2 -1  0  1  2  3  4))
(defparameter b (reshape a '(3 3)))


(deftest unary-norm-infinity-vector (unary-operations)
  (assert-true (num= 6   (linear-algebra-kernel:norm-vector #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) :infinity)))
  (assert-true (num= 4.0 (linear-algebra-kernel:norm-vector #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
							      #C(-2 3) #C(-3 1) #C(-1 0)) :infinity))))

(defparameter A #2A((1 2)
                    (3 4)))
(defparameter B #2A((5 6)
                    (7 8)))

1 2 | 5 6
3 4 | 7 8

(aops:each-index (i j)
   (aops:sum-index k
      (* (aref A i k) (aref B k j)))) ; => #2A((19 22)
					;        (43 50))

https://algebra1course.wordpress.com/2013/02/19/3-matrix-operations-dot-products-and-inverses/
(defun m. (a b)
  "Dot product of two matrices"
  (aops:each-index (i j)
   (aops:sum-index k
      (* (aref A i k) (aref B k j)))))


(defun foo ()
  ""
  t)


#|"will this work" Test of documentation with embedded quotation marks |#

grep -ri --exclude-dir={documentation,c2ffi,cl-duckdb,electron-display,third-party,duim,plotview} dita



;; Printing a data frame
(print-matrix (as-array mtcars) *standard-output* :column-labels (keys mtcars))

;;    MODEL                MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;;  0 Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
;;  1 Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
;;  2 Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
;;  3 Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
;;  4 Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
;;  5 Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
;;  6 Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
;;  7 Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
;;  8 Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
;;  9 Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
;; 10 Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
;; 11 Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
;; 12 Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
;; 13 Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
;; 14 Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
;; 15 Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
;; 16 Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
;; 17 Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
;; 18 Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
;; 19 Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
;; 20 Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
;; 21 Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
;; 22 AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
;; 23 Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
;; 24 Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2 ..
