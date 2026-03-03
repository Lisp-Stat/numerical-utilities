---
name: clunit2-testing
description: 'clunit2 Common Lisp test framework API reference and patterns. Use when writing or fixing tests.'
---

# clunit2 Test Framework

[clunit2](https://codeberg.org/cage/clunit2) is a Common Lisp unit testing framework. Package: `:clunit`.

## Test Suites

```lisp
;; Define a root suite (no parents)
(clunit:defsuite my-suite ())

;; Define a child suite
(clunit:defsuite child-suite (my-suite))

;; Multiple parent suites (multiple inheritance)
(clunit:defsuite shared-suite (suite-a suite-b))
```

## Test Definitions

```lisp
(deftest test-name (parent-suite)
  "Mandatory docstring describing the behavior under test."
  ;; test body with assertions
  (assert-true (some-predicate)))
```

Every `deftest` **must** have a docstring. Omitting it is a reviewer FAIL.

## Assertions

| Macro | Comparison | Use When |
|-------|-----------|----------|
| `(assert-true expr)` | `expr` is non-NIL | Boolean checks |
| `(assert-false expr)` | `expr` is NIL | Negative checks |
| `(assert-eq expected actual)` | `EQ` | Symbol identity |
| `(assert-eql expected actual)` | `EQL` | Numbers, characters |
| `(assert-equal expected actual)` | `EQUAL` | Lists, strings |
| `(assert-equalp expected actual)` | `EQUALP` | Hash-tables, case-insensitive strings, arrays |
| `(assert-equality test expected actual)` | `(funcall test ...)` | Custom predicates |
| `(assert-condition condition expr)` | Signals `condition` | Error/warning testing |
| `(assert-finishes expr)` | No error signaled | Smoke tests |
| `(assert-fails format-string)` | Force failure | Placeholder/todo |

## Running Tests

```lisp
;; Run a suite (returns a clunit-report object)
(clunit:run-suite 'my-suite)

;; Run with progress reporting
(clunit:run-suite 'my-suite :report-progress t)

;; Run a single test
(clunit:run-test 'test-name)
```

`run-suite` returns a `CLUNIT-REPORT` object. Output is written to
`clunit:*test-output-stream*`.

## Known Issues

### 1. `*test-output-stream*` initialized to a null stream (ql:quickload :silent t)

**Root cause**: `(defvar *test-output-stream* *standard-output*)` in clunit2's
`specials.lisp` captures `*standard-output*` at load time. When clunit2 is
loaded via `(ql:quickload :clunit2 :silent t)`, Quicklisp binds
`*standard-output*` to a null `BROADCAST-STREAM`. All output is discarded.

**Fix**: Rebind `clunit:*test-output-stream*` to `*standard-output*` before
running tests:

```lisp
(defun run-tests (&optional (report-progress t))
  (let ((*print-pretty* t)
        (clunit:*test-output-stream* *standard-output*))
    (clunit:run-suite 'all-tests :report-progress report-progress)))
```

### 2. `*print-pretty*` nil causes report to print on a single line

Bind `*print-pretty* t` as shown above.

## Test File Style Rules

### Helper Placement
All helper `defun`s must be grouped together near the **top** of the test file,
before any test suite definitions. Never scatter helpers between test suites.

### Internal Symbol Isolation
When a test needs access to internal (`::`) symbols from a dependency package,
wrap the access in a named helper function with a `NOTE` comment:

```lisp
;;; NOTE: Uses internal function from num-utils.elementwise — update if API changes.
(defun call-float-contagion (&rest objects)
  (apply #'num-utils.elementwise::elementwise-float-contagion objects))
```

### Float Literals
Always use the `d0` suffix on every float literal in test data:

```lisp
;; GOOD
(make-array 3 :element-type 'double-float :initial-contents '(1.0d0 2.0d0 3.0d0))

;; BAD — bare 1.0 is a single-float
(make-array 3 :element-type 'double-float :initial-contents '(1.0 2.0 3.0))
```

### Sequential Bindings
Use `let*` instead of nested `let` when bindings depend on each other:

```lisp
;; GOOD
(let* ((result (some-function input))
       (parsed (process result)))
  ...)

;; BAD — unnecessary nesting
(let ((result (some-function input)))
  (let ((parsed (process result)))
    ...))
```

<!-- BEGIN PROJECT-SPECIFIC -->

## Patterns for num-utils

### FiveAM Migration Notes
num-utils was migrated from FiveAM to clunit2. Key mapping:

| FiveAM | clunit2 |
|--------|---------|
| `(def-suite name :in parent)` | `(defsuite name (parent))` |
| `(in-suite name)` | *(not needed — suite is arg to deftest)* |
| `(test name body...)` | `(deftest name (suite) "docstring" body...)` |
| `(is expr)` | `(assert-true expr)` |
| `(is (= a b))` | `(assert-eql a b)` |
| `(is (equalp a b))` | `(assert-equalp a b)` |
| `(signals condition expr)` | `(assert-condition condition expr)` |

**Latent bug fix**: FiveAM's bare `(not ...)` forms (without `is`) are silent
no-ops. Migration replaces them with `(assert-false ...)`.

### Numeric Comparisons
Use `num=` for floating-point assertions:

```lisp
(assert-true (num= expected actual))
(assert-true (num= expected actual 1d-7))  ; with tolerance
```

### Array Construction Helper
```lisp
(flet ((arr (dimensions element-type &rest elements)
         (aprog1 (make-array dimensions :element-type element-type)
           (loop for index from 0
                 for element in elements
                 do (setf (row-major-aref it index)
                          (coerce element element-type))))))
  (let ((a (arr '(2 3) 'double-float 1 2 3 4 5 6)))
    ...))
```

### ASDF Integration
```lisp
(defsystem "num-utils/tests"
  :depends-on ("num-utils" "clunit2")
  :perform (test-op (o s)
            (symbol-call :num-utils-tests :run-tests)))
```

<!-- END PROJECT-SPECIFIC -->
