# Copilot Instructions for num-utils (Lisp-Stat)

## Project Overview
This is `num-utils`, a Common Lisp numerical utilities library in the [Lisp-Stat](https://lisp-stat.dev/) ecosystem. It provides arithmetic, elementwise array operations, intervals, matrices, polynomials, rootfinding, quadrature, and more.

### Subsystems
- **num-utils.arithmetic** — scalar math: `square`, `cube`, `numseq`, `ivec`, `sum`, `product`, cumulative ops
- **num-utils.elementwise** — vectorized array ops: `e+`, `e-`, `e*`, `e/`, trig, rounding, comparisons
- **num-utils.num=** — approximate numeric equality with configurable tolerance
- **num-utils.interval** — interval arithmetic and representation
- **num-utils.matrix** — matrix construction and decomposition utilities
- **num-utils.matrix-shorthand** — concise matrix literal syntax
- **num-utils.chebyshev** — Chebyshev polynomial approximation
- **num-utils.polynomial** — polynomial evaluation and manipulation
- **num-utils.rootfinding** — univariate rootfinding (bisection, etc.)
- **num-utils.quadrature** — numerical integration
- **num-utils.log-exp** — log/exp utility functions
- **num-utils.print-matrix** — matrix pretty-printing
- **num-utils.utilities** — shared low-level utilities
- **num-utils.test-utilities** — test helpers (error accumulators, etc.)

<!-- BEGIN PROJECT-SPECIFIC -->

### Runtime
- **Common Lisp implementation**: SBCL (Steel Bank Common Lisp)
- **Build system**: ASDF
- **Test framework**: clunit2
- **Package nickname**: `nu` (for `num-utils`), `elmt` (for `num-utils.elementwise`)

### Key Conventions
- Packages use `uiop:define-package` with explicit `:export` lists
- The umbrella package `num-utils` (nickname `nu`) re-exports all subsystem symbols via `:use-reexport`
- Elementwise operations follow a naming convention:
  - `e1<op>` — unary (e.g., `e1log`, `eabs`, `esqrt`)
  - `e2<op>` — binary (e.g., `e2+`, `e2log`, `eexpt`)
  - `e<op>` — variadic wrapper dispatching to e1/e2 (e.g., `e+`, `e-`, `elog`)
- Three macros define the elementwise system: `define-e1`, `define-e2`, `define-e&`
- `elementwise-float-contagion` determines result array element-type
- `mapping-array` macro iterates arrays with `row-major-aref`
- Use `num=` for floating-point comparisons in tests

### Test Constraints
- Tests must be deterministic — no randomness, no timing dependencies
- Use `num=` (with appropriate tolerance) for floating-point comparisons
- Array comparisons use `equalp` for exact or `num=` element-by-element for approximate
- Test files are in `tests/` directory
- The test package `num-utils-tests` uses clunit2

### Running Tests
```sh
cd /workspace && sbcl --non-interactive \
  --eval '(push #p"/workspace/" asdf:*central-registry*)' \
  --eval '(ql:quickload :num-utils :silent t)' \
  --eval '(ql:quickload :clunit2 :silent t)' \
  --eval '(asdf:test-system "num-utils")'
```

### Key Source Files
- [src/elementwise.lisp](src/elementwise.lisp) — `define-e1`, `define-e2`, `define-e&`, all vectorized ops
- [src/arithmetic.lisp](src/arithmetic.lisp) — `square`, `cube`, `numseq`, `ivec`, `sum`, `product`
- [src/num=.lisp](src/num=.lisp) — `num=`, approximate numeric equality
- [src/utilities.lisp](src/utilities.lisp) — shared utility functions
- [src/pkgdcl.lisp](src/pkgdcl.lisp) — umbrella package re-exporting all subsystems
- [tests/test-package.lisp](tests/test-package.lisp) — test package declaration
- [tests/main.lisp](tests/main.lisp) — test runner and root suite
- [tests/elementwise.lisp](tests/elementwise.lisp) — elementwise operation tests

### How to Run the RALPH Development Loop

This project uses the **RALPH** (Red→Analyze→Loop→Plan→Hypothesize) iterative development cycle, driven by `prd.json` (product backlog) and `progress.txt` (accumulated learnings).

**Two entry points** — both drive from the same `prd.json` and `progress.txt`. Do not run both simultaneously.

#### CLI (headless / terminal)
```sh
./ralph.sh                          # default: claude-sonnet-4.6, 30 iterations
./ralph.sh 10                       # limit to 10 iterations
./ralph.sh --model claude-opus-4.6  # use a different model
```

#### VS Code Copilot Chat (Agent Mode)

| Action | How to invoke |
|--------|---------------|
| **Start RALPH loop** | Use the `ralph-loop` prompt — or `@ralph` agent |
| **Resume after reset** | Use the `continue-work` prompt |
| **Run tests only** | Use the `run-tests` prompt |
| **Write a new test** | Use the `write-test` prompt |

The `@ralph` agent orchestrates the full cycle and delegates to `@analyst`, `@test-engineer`, and `@reviewer` sub-agents as needed. It reads `prd.json` to pick the next unfinished story, implements it, runs tests, fixes failures, commits, and moves to the next story.

### Continuation: Current Development Focus

**Current state**: Completing vectorized math operators (Tier 1+2) and migrating tests from FiveAM to clunit2.

**Elementwise operator tiers**:
- **Tier 1** (high priority): trig family (tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh), rounding (round, truncate + 2-arg forms), signum, /=, rem
- **Tier 2** (scientific computing): complex accessors (phase, realpart, imagpart, cis, complex), float-rounding (ffloor, fceiling, fround, ftruncate), binary max/min

**Test migration**: All test files migrating from FiveAM to clunit2. Fix latent `(not ...)` non-assertion bugs discovered in existing tests.

<!-- END PROJECT-SPECIFIC -->
