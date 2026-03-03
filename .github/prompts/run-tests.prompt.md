---
description: 'Run the test suite and report pass/fail results'
name: run-tests
tools:
  - runInTerminal
---

# Run Tests

Run the test suite using SBCL and report the results.

<!-- BEGIN PROJECT-SPECIFIC -->
## Command
```sh
cd /workspace && sbcl --non-interactive \
  --eval '(push #p"/workspace/" asdf:*central-registry*)' \
  --eval '(ql:quickload :num-utils :silent t)' \
  --eval '(ql:quickload :clunit2 :silent t)' \
  --eval '(asdf:test-system "num-utils")'
```
<!-- END PROJECT-SPECIFIC -->

## Parse Output
Look for clunit2 output lines containing:
- `PASS` / `FAIL` counts
- Individual test results
- Any compilation errors or warnings

Report:
1. Total tests run
2. Tests passed
3. Tests failed (with names and failure messages)
4. Any load/compilation errors
