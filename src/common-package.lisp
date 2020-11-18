;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:num-utils)
    (defpackage #:num-utils
      (:nicknames #:clnu)
      (:use #:cl))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package #:num-utils)

  (flet ((reexport (package)
           "Reexport all external symbols of package."
           (let ((package (find-package package)))
             (do-external-symbols (symbol package)
               (when (eq (symbol-package symbol) package)
                 (import symbol)
                 (export symbol))))))
    (reexport '#:num-utils.arithmetic)
    (reexport '#:num-utils.chebyshev)
    (reexport '#:num-utils.elementwise)
    (reexport '#:num-utils.interval)
    (reexport '#:num-utils.matrix)
    (reexport '#:num-utils.num=)
    (reexport '#:num-utils.statistics)
    (reexport '#:num-utils.utilities)
    (reexport '#:num-utils.rootfinding)
    (reexport '#:num-utils.quadrature)))
