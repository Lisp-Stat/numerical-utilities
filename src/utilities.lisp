;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: NUM-UTILS.UTILITIES -*-
(cl:in-package :num-utils.utilities)

(defmacro gethash* (key hash-table
                    &optional (datum "Key not found.")
                    &rest arguments)
  "Like GETHASH, but checking that KEY is present and raising the given error if not."
  (with-unique-names (value present?)
    `(multiple-value-bind (,value ,present?) (gethash ,key ,hash-table)
       (assert ,present? () ,datum ,@arguments)
       ,value)))

(defmacro splice-when (test &body forms)
  "Similar to when, but wraps the result in list.

Example: `(,foo ,@(splice-when add-bar? bar))"
  `(when ,test
     (list
      (progn ,@forms))))

(defmacro splice-awhen (test &body forms)
  "Similar to splice-when, but binds IT to test."
  `(awhen ,test
     (list
      (progn ,@forms))))

;;; Replace with alexandria curry?
(defmacro curry* (function &rest arguments)
  "Currying in all variables that are not *.  Note that this is a macro, so * should not be quoted, and FUNCTION will be used as is, ie it can be a LAMBDA form."
  (let ((arguments (loop for arg in arguments
                         collect (cond
                                   ((eq arg '*) (gensym "ARG"))
                                   ((keywordp arg) (list arg))
                                   (t (list (gensym "VAR") arg))))))
    `(let ,(loop for arg in arguments
                 when (and (consp arg) (cdr arg))
                 collect arg)
       (lambda ,(loop for arg in arguments
                      unless (consp arg)
                      collect arg)
         (,function ,@(loop for arg in arguments
                            collect (if (consp arg)
                                        (car arg)
                                        arg)))))))

(defmacro check-types ((&rest arguments) type)
  "CHECK-TYPE for multiple places of the same type.  Each argument is either a place, or a list of a place and a type-string."
  `(progn
     ,@(loop
         for argument :in arguments
         collecting (if (atom argument)
                        `(check-type ,argument ,type)
                        (let+ (((place type-string) argument))
                          `(check-type ,place ,type ,type-string))))))

(defmacro define-with-multiple-bindings
    (macro &key
           (plural (intern (format nil "~aS" macro)))
           (docstring (format nil "Multiple binding version of ~(~a~)." macro)))
  "Define a version of MACRO with multiple arguments, given as a list.  Application of MACRO will be nested.  The new name is the plural of the old one (generated using format by default)."
  `(defmacro ,plural (bindings &body body)
     ,docstring
     (if bindings
         `(,',macro ,(car bindings)
                    (,',plural ,(cdr bindings)
			       ,@body))
         `(progn ,@body))))

(defmacro unlessf (place value-form &environment environment)
  "When PLACE is NIL, evaluate VALUE-FORM and save it there."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    `(let* ,(mapcar #'list vars vals)
       (unless ,reader-form
         (let ((,(car store-vars) ,value-form))
           ,writer-form)))))

(declaim (inline within?))
(defun within? (left value right)
  "Return non-nil iff value is in [left,right)."
  (and (<= left value) (< value right)))

(declaim (inline fixnum?))
(defun fixnum? (object)
  "Check of type of OBJECT is fixnum."
  (typep object 'fixnum))

(deftype simple-fixnum-vector ()
  "Simple vector or fixnum elements."
  '(simple-array fixnum (*)))

(defun as-simple-fixnum-vector (sequence &optional copy?)
  "Convert SEQUENCE to a SIMPLE-FIXNUM-VECTOR.  When COPY?, make sure that the they don't share structure."
  (if (and (typep sequence 'simple-fixnum-vector) copy?)
      (copy-seq sequence)
      (coerce sequence 'simple-fixnum-vector)))

(defun as-double-float (v)
  "Convert argument to DOUBLE-FLOAT."
  (coerce v 'double-float))

(defmacro with-double-floats (bindings &body body)
  "For each binding = (variable value), coerce VALUE to DOUBLE-FLOAT and bind it to VARIABLE for BODY.  When VALUE is omitted, VARIABLE is used instead.  When BINDING is an atom, it is used for both the value and the variable.

Example:
  (with-double-floats (a
                       (b)
                       (c 1))
    ...)"
  `(let ,(mapcar (lambda (binding)
                   (let+ (((variable &optional (value variable))
                           (ensure-list binding)))
                     `(,variable (as-double-float ,value))))
                 bindings)
     ,@body))

(deftype simple-double-float-vector (&optional (length '*))
  "Simple vector of double-float elements."
  `(simple-array double-float (,length)))

(deftype simple-single-float-vector (&optional (length '*))
  "Simple vector of single-float elements."
  `(simple-array single-float (,length)))

(defun generate-sequence (result-type size function)
  "Like MAKE-SEQUENCE, but using a function to fill the result."
  (map-into (make-sequence result-type size) function))

(defmacro expanding (&body body)
  "Expand BODY. Useful for generating code programmatically."
  (with-gensyms (local-macro)
    `(macrolet ((,local-macro ()
                  ,@body))
       (,local-macro))))

(defun bic (a b)
  "Biconditional.  Returns A <=> B."
  (if a b (not b)))

(defun binary-search (sorted-reals value)
  "Return INDEX such that

  (WITHIN? (AREF SORTED-REALS INDEX) VALUE (AREF SORTED-REALS (1+ INDEX)).

SORTED-REALS is assumed to be reals sorted in ascending order (not checked, if this does not hold the result may be nonsensical, though the algorithm will terminate).

If value is below (or above) the first (last) break, NIL (T) is returned."
  (let+ ((left 0)
         (right (1- (length sorted-reals)))
         ((&flet sr (index) (aref sorted-reals index))))
    (cond
      ((< value (sr left)) nil)
      ((<= (sr right) value) t)
      (t (loop
           (when (= (1+ left) right)
             (return left))
           (let ((middle (floor (+ left right) 2)))
             (if (< value (sr middle))
                 (setf right middle)
                 (setf left middle))))))))

(defgeneric as-alist (object)
  (:documentation "Return OBJECT as an ALIST.  Semantics depends on OBJECT."))

(defgeneric as-plist (object)
  (:documentation "Return OBJECT as a PLIST.  Semantics depends on OBJECT.  The default method uses AS-ALIST.")
  (:method (object)
    (alist-plist (as-alist object))))

;; TODO Remove this, as it's now in Alexandria
(declaim (inline sequencep))
(defun sequencep (x)
  "Return T if X is type SEQUENCE."
  (typep x 'sequence))

(defun make-vector (element-type &rest initial-contents)
  (make-array (length initial-contents) :element-type element-type
              :initial-contents initial-contents))

(define-compiler-macro make-vector (element-type &rest initial-contents)
  `(let ((vec (make-array ,(length initial-contents)
                          :element-type ,element-type)))
     ,@(let ((i -1))
	    (mapcar (lambda (form)
		      `(setf (aref vec ,(incf i)) ,form))
		    initial-contents))
     vec))

