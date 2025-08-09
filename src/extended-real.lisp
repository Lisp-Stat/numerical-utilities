;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.EXTENDED-REAL -*-
;;; Copyright (c) 2011-2014 Tamas Papp
;;; Copyright (c) 2023, 2025 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:num-utils.extended-real
  (:use #:cl #:alexandria)
  (:nicknames #:xreal)
  (:shadow #:= #:< #:> #:<= #:>=)
  (:export
   :infinite?
   :extended-real
   :=
   :<
   :>
   :<=
   :>=
   :plusinf
   :minusinf
   :with-template
   :lambda-template)
   (:documentation "Extends the real number line with positive and negative infinity (:plusinf, :minusinf). Provides comparison operators (=, <, >, <=, >=) that work seamlessly with both real numbers and infinities. Includes type definitions for extended reals, infinity testing, and template macros for pattern matching on extended real values."))
(in-package #:num-utils.extended-real)


(deftype infinite ()
  "Representing infinity (extending the real line)."
  '(member :plusinf :minusinf))

(defun infinite? (object)
  "Test if an object represents positive or negative infinity."
  (typep object 'infinite))

(deftype extended-real (&optional (base 'real))
  "Extended real number."
  `(or infinite ,base))

(defun extend-pairwise-comparison (test first rest)
  "Extend TEST (a pairwise comparison) to an arbitrary number of arguments (but at least one, FIRST)."
  (loop while rest do
    (let ((next (car rest)))
      (unless (funcall test first next)
        (return-from extend-pairwise-comparison nil))
      (setf first next
            rest (cdr rest))))
  t)

(defmacro with-template ((prefix &rest variables) &body body)
  "Define the function (PREFIX &rest VARIABLES) which can be used to match variables using :PLUSINF, :MINUSINF, REAL, or T."
  (let ((names (mapcar (curry #'symbolicate 'kind-) variables)))
    `(macrolet ((,prefix ,names
                  (flet ((expand (kind variable)
                           (ecase kind
                             (:plusinf `(eq :plusinf ,variable))
                             (:minusinf `(eq :minusinf ,variable))
                             (real `(realp ,variable))
                             ((t) t))))
                    (list 'and
                          ,@(mapcar (lambda (name variable)
                                      `(expand ,name ',variable))
                                    names variables)))))
       ,@(loop for v in variables
               collect `(check-type ,v extended-real))
       ,@body)))

(defmacro lambda-template ((prefix &rest variables) &body body)
  "LAMBDA with WITH-TEMPLATE in its BODY."
  `(lambda ,variables
     (with-template (,prefix ,@variables)
       ,@body)))

(defmacro define-comparison (name test)
  "Define a comparison, extendeding a pairwise comparison to an arbitrary number of arguments."
  `(defun ,name (number &rest more-numbers)
     (extend-pairwise-comparison ,test number more-numbers)))

(define-comparison =
    (lambda-template (? a b)
      (if (? real real)
          (cl:= a b)
          (or (? :plusinf :plusinf)
              (? :minusinf :minusinf)))))

(define-comparison <
    (lambda-template (? a b)
      (if (? real real)
          (cl:< a b)
          (or (? :minusinf :plusinf)
              (? :minusinf real)
              (? real :plusinf)))))

(define-comparison >
    (lambda-template (? a b)
      (if (? real real)
          (cl:> a b)
          (or (? :plusinf :minusinf)
              (? real :minusinf)
              (? :plusinf real)))))

(define-comparison <=
    (lambda-template (? a b)
      (if (? real real)
          (cl:<= a b)
          (or (? :minusinf t)
              (? t :plusinf)))))

(define-comparison >=
    (lambda-template (? a b)
      (if (? real real)
          (cl:>= a b)
          (or (? t :minusinf)
              (? :plusinf t)))))

;;; TODO /=, min, max, minusp, plusp, abs, ...
