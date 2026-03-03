# RALPH Development Loop — Usage Guide

This project uses the **RALPH** (Red→Analyze→Loop→Plan→Hypothesize) iterative development cycle for test-driven implementation of num-utils features.

## Prerequisites

- This devcontainer running with SBCL and Quicklisp pre-installed
- **For VS Code mode**: GitHub Copilot Chat extension (agent mode enabled)
- **For CLI mode**: GitHub Copilot CLI (`copilot`) installed and authenticated

## Quick Start — CLI Mode

Run the RALPH loop from the terminal:

```sh
./ralph.sh                          # default: claude-sonnet-4.6, 30 iterations
./ralph.sh 10                       # limit to 10 iterations
./ralph.sh --model claude-opus-4.6  # use a different model
./ralph.sh --model gpt-5.2-codex 5  # different model, 5 iterations
```

The script runs `copilot -p` in a non-interactive loop. Between iterations it checks `prd.json` — when all stories have `passes: true`, it exits. Each iteration the AI agent reads the backlog, picks the next story, implements it, runs tests, and commits.

Flags: `--yolo` (auto-approve all tools/paths), `--no-ask-user` (fully autonomous), `--add-dir` (project path access).

## Quick Start — VS Code Mode

Open Copilot Chat in **Agent Mode** (click the agent icon or use `Ctrl+Shift+I`), then:

### Start a new session
Type: **`@ralph`** or use the **`ralph-loop`** prompt

This will:
1. Read `prd.json` and pick the highest-priority unfinished story
2. Implement the feature or fix
3. Run the test suite
4. Iterate on failures (up to 10 times)
5. On green: update `prd.json`, append to `progress.txt`, commit
6. Pick up the next story if capacity remains

### Resume after a context reset
Use the **`continue-work`** prompt

This recovers state from `prd.json` and `progress.txt`, verifies the test harness, and picks up where you left off.

### Run tests only
Use the **`run-tests`** prompt

Runs the full test suite and reports pass/fail counts without making changes.

### Write a specific test
Use the **`write-test`** prompt

Prompts for a function name and generates a clunit2 test following project conventions.

## Agent Architecture

```
@ralph (orchestrator)
  ├── @analyst     — reads source, explains failure root causes
  ├── @test-engineer — writes/fixes clunit2 tests
  └── @reviewer    — validates CL conventions and test quality
```

## Key Files

| File | Purpose |
|------|---------|
| `prd.json` | Product backlog — user stories with `passes: true/false` |
| `progress.txt` | Accumulated learnings and codebase patterns |
| `.github/agents/ralph.agent.md` | RALPH orchestrator agent definition |
| `.github/prompts/ralph-loop.prompt.md` | Full RALPH cycle prompt |
| `.github/prompts/continue-work.prompt.md` | Resume-after-reset prompt |
| `.github/prompts/run-tests.prompt.md` | Test runner prompt |
| `.github/prompts/write-test.prompt.md` | Test writer prompt |
| `.github/skills/cl-elementwise/SKILL.md` | Elementwise architecture reference |
| `.github/skills/clunit2-testing/SKILL.md` | clunit2 API reference |

## Dual-Mode Architecture

Both the CLI (`ralph.sh`) and VS Code (`@ralph` agent) drive from the same `prd.json` and `progress.txt`. They are **equal peers** — use whichever is convenient, but do not run both simultaneously.

| Mode | Best for |
|------|----------|
| **CLI** (`./ralph.sh`) | Headless/background runs, CI, long batch sessions |
| **VS Code** (`@ralph`) | Interactive development, debugging, one-off fixes |

The original `ralph.sh.bak` used the Anthropic Claude CLI (`claude --print`). The current `ralph.sh` uses the GitHub Copilot CLI (`copilot -p`).
