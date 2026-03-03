---
name: 'Common Lisp Conventions'
description: 'Coding style and conventions for Common Lisp source files'
applyTo: '**/*.lisp'
---

# Common Lisp Coding Conventions

## File Header
Every `.lisp` file must start with an Emacs mode line and copyright notice:
```lisp
;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: PACKAGE-NAME -*-
;;; Copyright (c) YEAR by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
```

## Package Declaration
- Use `uiop:define-package` (not `defpackage`)
- Use uninterned symbols for package names: `#:vega`, `#:plot`
- List all exports explicitly with `(:export ...)`

## Naming
- Special/dynamic variables: earmuff convention `*variable-name*`
- Use `defparameter` for variables that should be re-evaluated on reload
- Use `defvar` for variables that should keep their value on reload
- Predicate functions end in `p` (no hyphen for single-word, hyphen for multi-word): `plistp`, `is-valid-p`

## Formatting
- No tabs — use spaces only
- 2-space indent for body forms
- Align `let` bindings vertically
- Close parentheses on the same line (no Algol-style closing)
- Use semicolons for comments: `;;;` for top-level, `;;` for code-level, `;` for inline
- Use `let+` instead of nested `let` when bindings are sequential

## Numeric Literals
- Always use the `d0` suffix for double-float literals: `0.1d0`, `1.0d0`
- Never use bare `1.0` or `0.1` — these create single-floats which can cause
  IEEE 754 representation surprises (e.g., Yason encodes `0.1` as `0.10000000149011612`)

## Style
- Prefer `let+` (from `let-plus`) for destructuring binds
- Use `alexandria` and `alexandria+` utility functions where available
- Use keyword arguments for optional parameters with defaults
- Signal conditions rather than returning error codes
- Use `check-type` and `assert` for input validation
- Group helper functions together near the top of a file, not scattered between
  definitions that use them
- Isolate uses of internal (`::`) symbols from dependency packages into named
  helper functions with a `NOTE` comment documenting the fragility
