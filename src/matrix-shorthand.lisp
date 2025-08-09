;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.MATRIX-SHORTHAND -*-
;;; Copyright (c) 2011-2014 Tamas Papp
;;; Copyright (c) 2023, 2025 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:num-utils.matrix-shorthand
  (:nicknames #:nu.mx)
  (:use #:cl
        #:alexandria
        #:anaphora
        #:num-utils.matrix
        #:num-utils.utilities
        #:let-plus)
  (:export
   #:vec
   #:mx
   #:diagonal-mx
   #:lower-triangular-mx
   #:hermitian-mx
   #:upper-triangular-mx)
   (:documentation "Provides convenient shorthand functions and macros for creating matrices and vectors. Includes vec for vectors, mx for dense matrices, and specialized constructors for diagonal, lower-triangular, upper-triangular, and Hermitian matrices. All functions support element type coercion and the macros offer concise syntax for matrix construction with automatic padding where needed."))
(in-package #:num-utils.matrix-shorthand)


(defun vec (element-type &rest elements)
  "Return a vector with elements coerced to ELEMENT-TYPE."
  (map `(simple-array ,element-type (*))
       (lambda (element) (coerce element element-type))
       elements))

(defun diagonal-mx (element-type &rest elements)
  "Return a DIAGONAL-MATRIX with elements coerced to ELEMENT-TYPE."
  (diagonal-matrix (apply #'vec element-type elements)))

(defmacro mx (element-type &body rows)
  "Macro for creating a dense matrix (ie a rank 2 array).  ROWS should be a list of lists (or atoms, which are treated as lists), elements are evaluated."
  (let+ ((rows (map 'vector #'ensure-list rows))
         (nrow (length rows))
         (ncol (length (aref rows 0)))
         ((&once-only element-type)))
    `(make-array (list ,nrow ,ncol)
                 :element-type ,element-type
                 :initial-contents
                 (list
                  ,@(loop for row across rows collect
                             `(list
                               ,@(loop for element in row collect
                                          `(coerce ,element ,element-type))))))))

(defun pad-left-expansion (rows ncol)
  "Pad ragged-right rows.  Used internally to implement ragged right matrix specifications."
  (loop for row in rows
        for row-index from 0
        collect (aprog1 (make-sequence 'list ncol :initial-element 0)
                  (replace it row :start1 0 :end1 (min ncol (1+ row-index))))))

(defmacro lower-triangular-mx (element-type &body rows)
  "Create a lower triangular matrix.  ROWS should be a list of lists, elements are evaluated.  Masked elements (above the diagonal) are ignored at the expansion, rows which don't have enough elements are padded with zeros."
  `(lower-triangular-matrix
    (mx ,element-type
      ,@(pad-left-expansion (mapcar #'ensure-list rows)
                            (reduce #'max rows :key #'length)))))

(defmacro hermitian-mx (element-type &body rows)
  "Create a lower triangular matrix.  ROWS should be a list of lists, elements are evaluated.  Masked elements (above the diagonal) are ignored at the expansion, rows which don't have enough elements are padded with zeros."
  `(hermitian-matrix
    (mx ,element-type
      ,@(pad-left-expansion (mapcar #'ensure-list rows)
                            (max (length rows)
                                 (reduce #'max rows :key #'length))))))

(defmacro upper-triangular-mx (element-type &body rows)
  "Create an upper triangular matrix.  ROWS should be a list of lists, elements are evaluated.  Masked elements (below the diagonal) are ignored at the expansion."
  `(upper-triangular-matrix
    (mx ,element-type
      ,@(loop for row-index from 0
              for row in rows
              collect (loop for column-index from 0
                            for element in (ensure-list row)
                            collect (if (< column-index row-index)
                                        0
                                        element))))))
