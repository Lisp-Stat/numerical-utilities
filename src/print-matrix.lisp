;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.PRINT-MATRIX -*-
;;; Copyright (c) 2011-2014 Tamas Papp
;;; Copyright (c) 2023, 2026 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:num-utils.print-matrix
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:print-length-truncate
   #:*print-matrix-precision*
   #:print-matrix)
  (:documentation "Provides formatted printing of 2D matrices with configurable precision, alignment, and truncation. Features include column alignment, custom element formatting, masking specific elements, respecting *print-length* for large matrices, and special handling for complex numbers. Supports customizable padding, indentation, and precision control through *print-matrix-precision* for human-readable matrix display."))
(in-package #:num-utils.print-matrix)

(defun print-length-truncate (dimension)
  "Return values (min dimension *print-length*) and whether the constraint is binding."
  (if (or (not *print-length*) (<= dimension *print-length*))
      (values dimension nil)
      (values *print-length* t)))

(defvar *print-matrix-precision* 5
  "Number of digits after the decimal point when printing numeric matrices.")

(defun decimal-precision (x &optional (max-precision *print-matrix-precision*))
  "Determine the number of significant decimal places needed for real number X.
Returns the minimum precision required, up to MAX-PRECISION."
  (when (realp x)
    (let* ((str (format nil "~,vf" max-precision (abs (float x 1.0d0))))
           (dot-pos (position #\. str)))
      (if dot-pos
          ;; Count from end, stopping at first non-zero digit after decimal point
          (loop for i from (1- (length str)) downto (1+ dot-pos)
                for char = (char str i)
                while (char= char #\0)
                finally (return (max 0 (- i dot-pos))))
          0))))

(defun print-matrix-formatter (x)
  "Standard formatter for matrix printing.  Respects *print-matrix-precision*, and formats complex numbers as a+bi, eg 0.0+1.0i.
Returns (values formatted-string alignment) where alignment is :LEFT, :RIGHT, or :CENTER.
All numeric types are right-aligned. Integers are formatted as floats if *print-matrix-precision* > 0."
  (let ((precision *print-matrix-precision*))
    (typecase x
      (integer
       (if (plusp precision)
           ;; Format as float if in a column with real numbers
           (values (format nil "~,vf" precision (float x 1.0d0)) :right)
           (values (format nil "~d" x) :right)))
      (real (values (format nil "~,vf" precision x) :right))
      (complex (values (format nil "~,vf+~,vfi"
                               precision (realpart x)
                               precision (imagpart x))
                       :right))
      (t (values (format nil "~a" x) :right)))))

(defun print-matrix (matrix stream
                     &key (formatter #'print-matrix-formatter)
                          (masked-fn (constantly nil))
                          (aligned? t)
                          (padding " ")
                          (indent "  ")
                          (column-labels nil)
                          (row-labels nil))
  "Format and print the elements of MATRIX (a 2d array) to STREAM, using PADDING between columns.

FORMATTER is called on each element and should return (values formatted-string alignment) where alignment is :LEFT, :RIGHT, :CENTER, or NIL (defaults to :RIGHT). For backward compatibility, formatters returning a single string value default to :RIGHT alignment.

MASKED-FN is called on row and column indices.  If it returns nil, the corresponding element is formatted using FORMATTER and printed.  Otherwise, it should return a string, which is printed as is.  INDENT is printed before each row.

If ALIGNED?, columns will be aligned according to the formatter's alignment specification. At most *PRINT-LENGTH* rows and columns are printed, more is indicated with ellipses (...).

COLUMN-LABELS is an optional vector of labels (symbols or strings) for columns. If provided, must match the number of columns (after truncation).

ROW-LABELS is an optional specification for row labels (always right-aligned).  Can be:
  - NIL (no row labels)
  - T (use 0-based integer indices)
  - A vector of labels (symbols, strings, or numbers) for each row"
  (let+ (((&values nrow row-trunc?) (print-length-truncate (aops:nrow matrix)))
	 ((&values ncol col-trunc?) (print-length-truncate (aops:ncol matrix)))
	 (formatted-elements (make-array (list nrow ncol)))
	 (column-widths (make-array ncol :element-type 'fixnum :initial-element 0))
	 (column-alignments (make-array ncol :initial-element :right))
	 (column-precisions (make-array ncol :element-type 'fixnum :initial-element 0)))
    ;; Validate and normalize labels
    (when column-labels
      (unless (= (length column-labels) ncol)
        (error "Column labels length (~D) does not match number of columns (~D)"
               (length column-labels) ncol)))
    (let ((normalized-row-labels
           (cond
             ((null row-labels) nil)
             ((eq row-labels t)
              ;; Generate 0-based indices
              (coerce (loop for i below nrow collect i) 'vector))
             (t
              ;; Use provided vector
              (unless (= (length row-labels) nrow)
                (error "Row labels length (~D) does not match number of rows (~D)"
                       (length row-labels) nrow))
              row-labels))))
      ;; Calculate row label width if present
      (let ((row-label-width
             (if normalized-row-labels
                 (loop for i below nrow
                       maximize (length (princ-to-string (aref normalized-row-labels i))))
                 0)))
        ;; Precision analysis pass - determine required precision per column
        (dotimes (col ncol)
          (let ((has-real nil))
            (dotimes (row nrow)
              (let ((element (aref matrix row col)))
                (when (and (realp element)
                           (not (integerp element))
                           (not (funcall masked-fn row col)))
                  (setf has-real t)
                  ;; Limit analysis to 2 decimals to avoid floating-point noise
                  (maxf (aref column-precisions col)
                        (decimal-precision element 2)))))
            ;; If column has any real numbers, ensure minimum precision of 1
            ;; so integers in that column are formatted as floats
            (when has-real
              (maxf (aref column-precisions col) 1))))
        ;; first pass - format elements, measure width
        (dotimes (col ncol)
          ;; Include column label width if present
          (when column-labels
            (maxf (aref column-widths col)
                  (length (princ-to-string (aref column-labels col)))))
          (dotimes (row nrow)
	    (let+ ((masked? (funcall masked-fn row col))
                   (formatted-element)
                   (alignment :right))
              (if masked?
                  (setf formatted-element masked?)
                  (let ((*print-matrix-precision* (aref column-precisions col)))
                    (multiple-value-bind (formatted align)
                        (funcall formatter (aref matrix row col))
                      (setf formatted-element formatted
                            alignment (or align :right)))))
              (let ((width (length formatted-element)))
                (maxf (aref column-widths col) width)
                (setf (aref formatted-elements row col) formatted-element))
              ;; Capture alignment from first row
              (when (zerop row)
                (setf (aref column-alignments col) alignment)))))
        ;; Print header row with column labels if present
        (when column-labels
          (format stream indent)
          ;; Print empty space for row label column
          (when normalized-row-labels
            (format stream "~V@A" row-label-width "")
            (princ padding stream))
          ;; Print column labels
          (dotimes (col ncol)
            (when (plusp col)
              (princ padding stream))
            (let ((label (princ-to-string (aref column-labels col))))
              (if aligned?
                  (let ((directive (case (aref column-alignments col)
                                    (:left "~VA")
                                    (:center "~V:@A")
                                    (otherwise "~V@A"))))
                    (format stream directive (aref column-widths col) label))
                  (princ label stream))))
          (when col-trunc?
            (princ padding stream)
            (princ "..." stream))
          (fresh-line stream))
        ;; second pass - print data rows
            (dotimes (row nrow)
          (when (or (plusp row) column-labels)
            (fresh-line stream))
          (format stream indent)
          ;; Print row label if present
          (when normalized-row-labels
            (format stream "~V@A" row-label-width
                    (princ-to-string (aref normalized-row-labels row)))
            (princ padding stream))
          (dotimes (col ncol)
	    (when (plusp col)
	      (princ padding stream))
	    (let ((elt (aref formatted-elements row col)))
	      (if aligned?
	          (let ((directive (case (aref column-alignments col)
                                    (:left "~VA")
                                    (:center "~V:@A")
                                    (otherwise "~V@A"))))
                    (format stream directive (aref column-widths col) elt))
	          (princ elt stream))))
          (when col-trunc?
	    (princ padding stream)
	    (princ "..." stream)))
        (when row-trunc?
          (format stream "~&..."))))))

;;; Sometimes we want an unwrapped matrix (rank 2 array) to be printed
;;; in human, as opposed to machine readable format.  For this we need
;;; to overide the implementation's print-object method.  Here's how to
;;; do that for SBCL:
#|
(defun output-matrix (array stream)
  (print-unreadable-object (array stream :type t)
    (format stream "~%")
      (print-matrix array stream)))

From SBCL:print.lisp
(defmethod sb-impl::print-object ((array array) stream)
  (if (and (or *print-array* *print-readably*) (array-element-type array))
      (if (= 2 (array-rank array))
	  (output-matrix array stream)
	  (sb-impl::output-array-guts array stream))
      (sb-impl::output-terse-array array stream)))
|#


(defun printer-status ()
  "Print values of all the printer variables"
  (format t ";;           *print-array* = ~a~%" *print-array*)
  (format t ";;            *print-base* = ~a~%" *print-base*)
  (format t ";;            *print-case* = ~a~%" *print-case*)
  (format t ";;          *print-circle* = ~a~%" *print-circle*)
  (format t ";;          *print-escape* = ~a~%" *print-escape*)
  (format t ";;          *print-gensym* = ~a~%" *print-gensym*)
  (format t ";;          *print-length* = ~a~%" *print-length*)
  (format t ";;           *print-level* = ~a~%" *print-level*)
  (format t ";;           *print-lines* = ~a~%" *print-lines*)
  (format t ";;     *print-miser-width* = ~a~%" *print-miser-width*)
  (format t ";; *print-pprint-dispatch* = ~a~%" *print-pprint-dispatch*)
  (format t ";;          *print-pretty* = ~a~%" *print-pretty*)
  (format t ";;           *print-radix* = ~a~%" *print-radix*)
  (format t ";;        *print-readably* = ~a~%" *print-readably*)
  (format t ";;    *print-right-margin* = ~a~%" *print-right-margin*))
