---
description: 'Execute the full RALPH loop — pick next story from prd.json, implement, test, commit'
name: ralph-loop
agent: ralph
tools:
  - runInTerminal
  - editFiles
  - search
  - codebase
  - fetch
---

# RALPH Loop

Execute the full RALPH (Red→Analyze→Loop→Plan→Hypothesize) cycle, driven by the product backlog.

## Startup
1. Read `prd.json` — find all user stories where `passes` is `false`, sorted by `priority` ascending
2. Read `progress.txt` — absorb accumulated learnings and patterns
3. Pick the **highest-priority** story that is not yet passing

## Goal
Implement and verify the selected story. Iterate the RALPH cycle (up to 10 iterations) until its tests pass. Then mark the story as passing, commit, and — if capacity remains in this turn — pick up the next story.

<!-- BEGIN PROJECT-SPECIFIC -->
## Test Command
```sh
cd /workspace && sbcl --non-interactive \
  --eval '(push #p"/workspace/" asdf:*central-registry*)' \
  --eval '(ql:quickload :num-utils :silent t)' \
  --eval '(ql:quickload :clunit2 :silent t)' \
  --eval '(asdf:test-system "num-utils")'
```
<!-- END PROJECT-SPECIFIC -->

## Cycle (per story)
1. **Red** — Run the test command and capture output
2. **Analyze** — If failures, delegate to `@analyst` for root cause analysis
3. **Plan** — Based on analysis, determine what to fix (test expectation, test construction, or source code)
4. **Hypothesize** — Delegate fix to `@test-engineer`
5. **Review** — Delegate to `@reviewer` for CL convention check
6. **Loop** — Re-run tests; repeat until green or 10 iterations

## On Green
When all tests pass for the current story:
1. Update `prd.json` — set `passes: true` for the completed story; add implementation notes
2. Append new learnings to `progress.txt`
3. Commit with message: `feat(US-XXX): <story title>`
4. Pick the next `passes: false` story and continue

## Constraints
- Tests must be deterministic
- Use clunit2 framework
- Max 10 RALPH iterations per story

## Report
After completion (or when stopping), summarize:
- Stories completed this session (IDs and titles)
- Number of RALPH iterations per story
- Final pass/fail status of the full test suite
- Next story to pick up
- Any remaining issues
