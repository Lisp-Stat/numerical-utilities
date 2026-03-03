---
description: 'Write a new clunit2 test for a specified function or behavior'
name: write-test
agent: test-engineer
tools:
  - editFiles
  - search
  - codebase
---

Write a new clunit2 test for: ${input:target:Function or behavior to test (e.g., etan, e2atan, efloor)}

## Process
1. **Read** the source implementation of the target function
2. **Understand** what it returns, its edge cases, and type behavior
3. **Create** tests in the appropriate test file and suite
4. Follow all Common Lisp and clunit2 conventions

## Test Template
```lisp
(deftest descriptive-name (appropriate-suite)
  "Docstring describing the behavior under test."
  (let* ((;; setup
          )
         (;; exercise
          )
         (;; verify
          ))
    (assert-equalp expected actual)))
```

## Constraints
- Tests must be deterministic — no randomness, no timing dependencies
- One behavior per test
- Every `deftest` **must** have a docstring
- All float literals must use the `d0` suffix (`1.0d0`, `0.5d0`) — never bare `1.0`
- Use `let*` for sequential bindings
- Place all helper `defun`s together near the top of the file
- Isolate uses of internal (`::`) symbols into named helper functions with a `NOTE` comment
- The test package `(:use :clunit)` — **never use the `clunit:` prefix** in test code

## Numeric Comparisons
Use `num=` for floating-point assertions:

```lisp
(assert-true (num= expected actual))       ; default tolerance
(assert-true (num= expected actual 1d-7))  ; explicit tolerance
```

<!-- BEGIN PROJECT-SPECIFIC -->

## Suite Hierarchy
```
all-tests
  elementwise
    elementwise-trig      — sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, etc.
    elementwise-rounding  — floor, ceiling, round, truncate, ffloor, etc.
    elementwise-binary    — +, -, *, /, expt, mod, rem, comparisons
    elementwise-complex   — phase, realpart, imagpart, cis, complex
    elementwise-reduction — ereduce, emax, emin
    elementwise-contagion — elementwise-float-contagion
```

## Key Source Files
- [src/elementwise.lisp](src/elementwise.lisp) — all elementwise operations
- [src/arithmetic.lisp](src/arithmetic.lisp) — `square`, used by `esquare`
- [src/num=.lisp](src/num=.lisp) — `num=`, approximate numeric equality

<!-- END PROJECT-SPECIFIC -->
