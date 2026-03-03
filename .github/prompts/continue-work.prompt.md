---
description: 'Resume RALPH development after devcontainer rebuild or context reset'
name: continue-work
agent: ralph
tools:
  - runInTerminal
  - editFiles
  - search
  - codebase
  - fetch
---

# Continue Development After Context Reset

This prompt resumes development work after a devcontainer rebuild or context reset.

## Steps

### 1. Check current state
- Read `prd.json` to see which user stories have `passes: true` vs `false`
- Read `progress.txt` for accumulated learnings and patterns
- Verify correct git branch (`ralph/elementwise-tier1-tier2`)
- Report: X of Y stories complete, next story is US-NNN

### 2. Verify test harness
<!-- BEGIN PROJECT-SPECIFIC -->
```sh
cd /workspace && sbcl --non-interactive \
  --eval '(push #p"/workspace/" asdf:*central-registry*)' \
  --eval '(ql:quickload :num-utils :silent t)' \
  --eval '(ql:quickload :clunit2 :silent t)' \
  --eval '(asdf:test-system "num-utils")'
```
<!-- END PROJECT-SPECIFIC -->

### 3. Pick up next story
Find the highest-priority story in `prd.json` where `passes: false` and begin implementation.

### 4. Follow RALPH loop
For each story:
1. Implement the change
2. Run tests
3. If failures, analyze and fix (up to 10 iterations)
4. When green:
   - Update `prd.json`: set `passes: true`, add notes
   - Append learnings to `progress.txt`
   - Commit: `feat(US-XXX): <story title>`
5. If capacity remains, immediately pick up the next `passes: false` story and continue

### 5. Before stopping
Report:
- Stories completed this session
- Current test suite status
- Next story to pick up
