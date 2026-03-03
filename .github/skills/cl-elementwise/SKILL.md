---
name: cl-elementwise
description: 'Elementwise vectorized math architecture — define-e1, define-e2, define-e&, mapping-array, float contagion. Use when implementing or testing vectorized operations.'
---

# Elementwise Vectorized Math Architecture

## Overview

The `num-utils.elementwise` package (nickname `elmt`) provides element-wise
mathematical operations that work uniformly on scalars, vectors, and
multi-dimensional arrays. Operations automatically handle type promotion
via `elementwise-float-contagion`.

## Macro System

Three macros define the entire operator hierarchy:

### `define-e1` — Unary Operations
```lisp
(define-e1 operation &key function docstring)
```
Generates a generic function with methods for `number` and `array`.
Default function name: `e1<operation>` (e.g., `e1log`, `e1-`).

```lisp
(define-e1 sin :function esin)
;; Generates:
;; (defgeneric esin (a)
;;   (:method ((a number)) (sin a))
;;   (:method ((a array))  (mapping-array (m a) (sin (m a)))))
```

### `define-e2` — Binary Operations
```lisp
(define-e2 operation &key function docstring)
```
Generates a generic function with methods for all combinations:
`(number, number)`, `(vector, number)`, `(number, vector)`, `(vector, vector)`,
`(array, number)`, `(number, array)`, `(array, array)`.
Default function name: `e2<operation>` (e.g., `e2+`, `e2log`).

Array-array methods assert equal dimensions.

### `define-e&` — Variadic Wrappers
```lisp
(define-e& operation &key function bivariate univariate docstring)
```
Generates a variadic function that reduces using the binary version,
or applies the unary version for single arguments.

```lisp
(define-e& + :univariate identity)
;; (e+ a)       → (identity a)
;; (e+ a b)     → (e2+ a b)
;; (e+ a b c)   → (e2+ (e2+ a b) c)
```

### `mapping-array` — Core Iteration Macro
```lisp
(mapping-array (ref array &rest other) form)
```
- Allocates result array with `elementwise-float-contagion` element type
- Iterates with `dotimes` + `row-major-aref`
- `ref` is a local function that extracts the current element from any array

## Naming Convention

| Type | Pattern | Examples |
|------|---------|----------|
| Unary | `e1<op>` or `e<op>` | `e1-`, `e1log`, `eabs`, `esqrt`, `esin` |
| Binary | `e2<op>` | `e2+`, `e2-`, `e2log`, `eexpt`, `emod` |
| Variadic | `e<op>` | `e+`, `e-`, `e*`, `e/`, `elog` |
| Reduction | `e<op>` | `emax`, `emin`, `ereduce` |

Note: Some unary ops use `e<op>` directly (e.g., `eabs`, `esqrt`, `esin`)
rather than `e1<op>` when no binary counterpart exists or when the binary
form uses a different base operation.

## Type Contagion

`elementwise-float-contagion` returns the promoted element type for the result
array. It uses a 10×10 lookup matrix encoding the CL type promotion hierarchy:

```
real < short-float < single-float < double-float < long-float
complex < (complex short-float) < ... < (complex long-float)
```

Mixed real/complex promotes to the complex variant of the wider float type.

## Multi-Value Functions

CL rounding functions (`floor`, `ceiling`, `round`, `truncate`) return two
values: quotient and remainder. The `define-e1`/`define-e2` macros capture
**only the primary value** because `setf row-major-aref` stores one value.
This is the correct behavior for vectorized rounding — users get the
quotient/rounded-value array. The `f`-variants (`ffloor`, etc.) return the
quotient as a float.

## Complete Operator Inventory

### Unary (`define-e1`)
**Existing**: `-`, `/`, `log`, `abs`, `floor`, `ceiling`, `exp`, `sqrt`, `conjugate`, `square`, `sin`, `cos`

**Tier 1 additions**: `tan`, `asin`, `acos`, `atan` (1-arg), `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `round`, `truncate`, `signum`

**Tier 2 additions**: `phase`, `realpart`, `imagpart`, `cis`, `ffloor`, `fceiling`, `fround`, `ftruncate`

### Binary (`define-e2`)
**Existing**: `+`, `-`, `*`, `/`, `expt`, `log`, `mod`, `<`, `<=`, `>`, `>=`, `=`

**Tier 1 additions**: `atan` (2-arg), `round`, `truncate`, `floor`, `ceiling`, `rem`, `/=`

**Tier 2 additions**: `complex`, `ffloor`, `fceiling`, `fround`, `ftruncate`, `max`, `min`

### Variadic (`define-e&`)
**Existing**: `+`, `-`, `*`, `/`

### Wrapper functions (like `elog`)
**Existing**: `elog` (dispatches `e1log`/`e2log`)
**Additions**: `eatan` (dispatches `e1atan`/`e2atan`)
