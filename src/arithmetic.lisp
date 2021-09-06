;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.ARITHMETIC -*-
(in-package #:num-utils.arithmetic)

;;; simple arithmetic

(define-modify-macro multf (coefficient) * "Multiply place by COEFFICIENT.")

(defun same-sign-p (&rest arguments)
  "Test whether all arguments have the same sign (ie all are positive, negative, or zero)."
  (if arguments
      (let+ (((first . rest) arguments)
             (sign (signum first)))
        (every (lambda (number) (= sign (signum number))) rest))
      t))

(declaim (inline square))
(defun square (number)
  "Square of number."
  (expt number 2))

(declaim (inline cube))
(defun cube (number)
  "Cube of number."
  (expt number 3))

(declaim (inline absolute-square))
(defun absolute-square (number)
  "Number multiplied by its complex conjugate."
  (* (conjugate number) number))

(declaim (inline abs-diff))
(defun abs-diff (a b)
  "Absolute difference of A and B."
  (abs (- a b)))

;;; Aliases for commonly used log bases.
;;; Why logarithms? The CL spec lets `log' return a single float for
;;; an integer argument, which is not what we want.
(declaim (inline log10 log2 ln))

(defun log10 (number)
  "Abbreviation for decimal logarithm."
  (log number 10d0))

(defun log2 (number)
  "Abbreviation for binary logarithm."
  (log number 2d0))

(defun ln (n)
  "Natural logarithm."
  (log n (exp 1d0)))


(declaim (inline 1c))

(defun 1c (number)
  "Return 1-number.  The mnemonic is \"1 complement\", 1- is already a CL
library function."
  (- 1 number))

(defun divides? (number divisor)
  "Test if DIVISOR divides NUMBER without remainder, and if so, return the
  quotient.  Works generally, but makes most sense for rationals."
  (let+ (((&values quot rem) (floor number divisor)))
    (when (zerop rem)
      quot)))

(defun as-integer (number)
  "If NUMBER represents an integer (as an integer, complex, or float, etc), return it as an integer, otherwise signal an error.  Floats are converted with RATIONALIZE."
  (declare (inline as-integer))
  (typecase number
    (integer number)
    (complex
     (assert (zerop (imagpart number)) ()
             "~A has non-zero imaginary part." number)
     (as-integer (realpart number)))
    (t (aprog1 (rationalize number)
         (assert (integerp it) () "~A has non-zero fractional part." number)))))


;;; arithmetic sequences

(defun sequence-minimum (x)
  "Return the minimum value in the sequence X"
  (check-type x alexandria:proper-sequence)
  (cond ((listp x) (apply 'min x))
	((vectorp x) (reduce #'min x))))

(defun sequence-maximum (x)
  "Return the maximum value in the sequence X"
  (check-type x alexandria:proper-sequence)
  (cond ((listp x) (apply 'max x))
	((vectorp x) (reduce #'max x))))

(defun numseq (from to &key length (by (unless length 1)) type)
  "Return a sequence between FROM and TO, progressing by BY, of the given LENGTH.  Only 3 of these a parameters should be given, the missing one (NIL) should be inferred automatically.  The sign of BY is adjusted if necessary.  If TYPE is LIST, the result is a list, otherwise it determines the element type of the resulting simple array.  If TYPE is nil, it as autodetected from the arguments (as a FIXNUM, a RATIONAL, or some subtype of FLOAT).  Note that the implementation may upgrade the element type."
  (flet ((seq% (from by length)
           (if (eq type 'list)
               (loop
                 for i :from 0 :below length
                 collecting (+ from (* i by)))
               (let+ ((type (cond
                              (type type)
                              ((= length 1) (if (typep from 'fixnum)
                                                'fixnum
                                                (type-of from)))
                              (t (let ((to (+ from (* by length))))
                                   (typecase to
                                     (fixnum (if (typep from 'fixnum)
                                                 'fixnum
                                                 'integer))
                                     (float (type-of to))
                                     (t 'rational))))))
                      (result (make-array length :element-type type)))
                 (dotimes (i length)
                   (setf (aref result i) (coerce (+ from (* i by)) type)))
                 result))))
    (cond
      ((not from)
       (seq% (- to (* by (1- length))) by length))
      ((not to)
       (seq% from by length))
      ((not length)
       (assert (not (zerop by)))
       (let* ((range (- to from))
	      (by (* (signum range) (signum by) by))
              (length (1+ (floor (/ range by)))))
         (seq% from by length)))
      ((and length (not by))
       (let ((range (- to from)))
         (seq% from (if (zerop range)
                        0
                        (/ range (1- length)))
               length)))
      (t (error "Only 3 of FROM, TO, LENGTH and BY are needed.")))))

(defun ivec (end-or-start &optional (end 0 end?) (by 1) strict-direction?)
  "Return a vector of fixnums.

   (ivec end) =>  #(0 ... end-1) (or #(0 ... end+1) when end is negative).

   (ivec start end) => #(start ... end-1) or to end+1 when end is negative.

When BY is given it determines the increment, adjusted to match the direction unless STRICT-DIRECTION, in which case an error is signalled. "
  (check-types (end-or-start end by) fixnum)
  (if end?
      (let* ((abs-by (abs by))
             (start end-or-start)
             (diff (- end start))
             (length (ceiling (abs diff) abs-by))
             (by (aprog1 (* abs-by (signum diff))
                   (when strict-direction?
                     (assert (= it by) () "BY does not match direction."))))
             (element start))
        (aprog1 (make-array length :element-type 'fixnum)
          (loop for index below length
                do (setf (aref it index) element)
                   (incf element by))))
      (let* ((end end-or-start)
             (abs-end (abs end)))
        (aprog1 (make-array abs-end :element-type 'fixnum)
          (if (plusp end)
              (loop for index below abs-end
                    do (setf (aref it index) index))
              (loop for index below abs-end
                    do (setf (aref it index) (- index))))))))

;;; sums and products

(defgeneric sum (object &key key)
  (:documentation "Sum of elements in object.  KEY is applied to each
  element.")
  (:method ((sequence sequence) &key (key #'identity))
    (reduce #'+ sequence :key key))
  (:method ((array array) &key (key #'identity))
    (reduce #'+ (aops:flatten array) :key key)))

(defgeneric product (object)
  (:documentation "Product of elements in object.")
  (:method ((sequence sequence))
    (reduce #'* sequence))
  (:method ((array array))
    (reduce #'* (aops:flatten array))))

;;; cumulative sum and product

(defun similar-element-type (element-type)
  "Return a type that is a supertype of ELEMENT-TYPE and is closed under arithmetic operations.  May not be the narrowest."
  (if (subtypep element-type 'float)
      element-type
      t))

(defun similar-sequence-type (sequence)
  "Return type that sequence can be mapped to using arithmetic operations."
  (etypecase sequence
    (list 'list)
    (vector `(simple-array
              ,(similar-element-type (array-element-type sequence)) (*)))))


(defun cumulative-sum (sequence
                       &key (result-type (similar-sequence-type sequence)))
  "Cumulative sum of sequence.  Return a sequence of the same kind and length; last element is the total.  The latter is returned as the second value."
  (let ((sum 0))
    (values (map result-type (lambda (element)
                               (incf sum element))
                 sequence)
            sum)))

(defun cumulative-product (sequence
                           &key (result-type
                                 (similar-sequence-type sequence)))
  "Cumulative product of sequence.  Return a sequence of the same kind and length; last element is the total product.  The latter is also returned as the second value."
  (let ((product 1))
    (values (map result-type (lambda (element)
                               (multf product element))
                 sequence)
            product)))

;;; norms

(defgeneric l2norm-square (object)
  (:documentation "Square of the $L_2$ norm of OBJECT.")
  (:method ((sequence sequence))
    (sum sequence :key #'absolute-square)))

(defun l2norm (object)
  "$L_2$ norm of OBJECT."
  (sqrt (l2norm-square object)))

(defun normalize-probabilities (vector
                                &key
                                (element-type t)
                                (result (make-array (length vector)
                                                    :element-type element-type)))
  "Verify that each element of VECTOR is nonnegative and return a vector multiplied so that they sum to 1.  ELEMENT-TYPE can be used to specify the element-type of the result.  When RESULT is given, the result is placed there.  When RESULT is NIL, VECTOR is modified instead."
  (unless result
    (setf result vector)
    (let ((result-type (array-element-type result)))
      (unless (subtypep element-type result-type)
        (setf element-type result-type))))
  (let ((result (aif result it vector))
        (sum (reduce #'+ vector
                     :key (lambda (element)
                            (assert (non-negative-real-p element))
                            element))))
    (map-into result (lambda (element) (coerce (/ element sum) element-type)) vector)))

;;; truncation/rounding

(defmacro define-rounding-with-offset (name function docstring)
  `(defun ,name (number &optional (divisor 1) (offset 0))
     ,docstring
     (let+ (((&values quotient remainder) (,function (- number offset) divisor)))
       (values (+ offset (* quotient divisor)) remainder))))

(define-rounding-with-offset floor* floor
  "Find the highest A=I*DIVISOR+OFFSET <= NUMBER, return (values A (- A NUMBER).")

(define-rounding-with-offset ceiling* ceiling
  "Find the lowest A=I*DIVISOR+OFFSET >= NUMBER, return (values A (- A NUMBER).")

(define-rounding-with-offset round* round
  "Find A=I*DIVISOR+OFFSET that minimizes |A-NUMBER|, return (values A (- A NUMBER).  When NUMBER is exactly in between two possible A's, the rounding rule of ROUND is used on NUMBER-OFFSET.")

(define-rounding-with-offset truncate* truncate
  "Find A=I*DIVISOR+OFFSET that maximizes |A|<=|NUMBER| with the same sign, return (values A (- A NUMBER).")
